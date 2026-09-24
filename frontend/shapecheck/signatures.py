"""Signatures of Python functions, from their annotations.

User functions follow jaxtyping: shape names and Python parameter names are
separate namespaces, a name that appears only in the return type is
existential, and asserts in the body are ignored (dropping them is sound).
The exception is an int parameter: a dim with its name is its value, so
subsequent_mask(size: int) -> Bool[Tensor, "1 size size"] says what it means.

Stubs follow the checker's own conventions: a shape may name an int
parameter (sum's `dim`), Dim["..."] is an int equal to a dim expression,
Shape["*S"] is a tuple of ints used as a shape, and asserts in the body are
preconditions, or postconditions when they mention an existential.
"""

from __future__ import annotations

import ast
import re
from collections.abc import Iterable
from dataclasses import dataclass

from . import ir
from .ir import Json
from .shapes import Scope, ShapeError, arith, is_int, parse_shape

# jaxtyping's dtype annotations, e.g. Float[Tensor, "b n"]
DTYPES = {
    "Shaped", "Num", "Real", "Inexact", "Integer", "Key",
    "Float", "Float16", "Float32", "Float64", "BFloat16",
    "Int", "Int8", "Int16", "Int32", "Int64",
    "UInt", "UInt8", "UInt16", "UInt32", "UInt64",
    "Complex", "Complex64", "Complex128", "Bool",
}  # fmt: skip
# array types with no shape annotation
ARRAY_TYPES = {"Tensor", "ndarray", "Array", "ArrayLike"}


class FrontendError(Exception):
    def __init__(self, message: str, node: ast.AST | None = None):
        super().__init__(message)
        self.message = message
        self.line = getattr(node, "lineno", None)


@dataclass
class Param:
    name: str  # the Python name, for keyword arguments
    ir_name: str
    kind: str  # "positional" (before /), "normal", "keyword" (after *), "varargs"
    default: Json | None  # a term, or None if the argument is required
    shape: bool  # Shape[...]: takes a tuple of ints, or collects *args


@dataclass
class Overload:
    sig: Json
    params: list[Param]


def last_name(node: ast.expr) -> str | None:
    if isinstance(node, ast.Name):
        return node.id
    if isinstance(node, ast.Attribute):
        return node.attr
    return None


def shape_strings(fn: ast.FunctionDef) -> Iterable[str]:
    """Every string inside the function's annotations, including those of
    annotated assignments in its body."""
    anns: list[ast.expr] = []
    a = fn.args
    for arg in a.posonlyargs + a.args + a.kwonlyargs + [a.vararg, a.kwarg]:
        if arg is not None and arg.annotation is not None:
            anns.append(arg.annotation)
    if fn.returns is not None:
        anns.append(fn.returns)
    anns += [s.annotation for s in ast.walk(fn) if isinstance(s, ast.AnnAssign)]
    for ann in anns:
        for node in ast.walk(ann):
            if isinstance(node, ast.Constant) and isinstance(node.value, str):
                yield node.value


def typ_of(ann: ast.expr | None, scope: Scope, binding: bool, what: str) -> tuple[Json, bool]:
    """The IR type of an annotation, and whether it's a Shape[...] (stubs)."""
    if ann is None:
        raise FrontendError(f"{what} needs an annotation")
    if isinstance(ann, ast.Constant) and isinstance(ann.value, str):
        # a forward reference, e.g. x: "Float[Tensor, 'n']"
        try:
            inner = ast.parse(ann.value, mode="eval").body
        except SyntaxError:
            raise FrontendError(f"can't parse the annotation of {what}", ann) from None
        return typ_of(ast.copy_location(inner, ann), scope, binding, what)
    try:
        return typ_of_ast(ann, scope, binding, what)
    except ShapeError as e:
        raise FrontendError(f"in the annotation of {what}: {e}", ann) from None


def typ_of_ast(ann: ast.expr, scope: Scope, binding: bool, what: str) -> tuple[Json, bool]:
    base = ann.value if isinstance(ann, ast.Subscript) else ann
    name = last_name(base)
    if not isinstance(ann, ast.Subscript):
        if name in ("int", "bool"):
            return ir.IntType(), False
        if name == "float":
            # a float broadcasts like a 0-d array
            return ir.Array([]), False
        if name in ARRAY_TYPES:
            if not binding:
                raise FrontendError(f'{what} needs a shape, e.g. Float[{name}, "n d"]', ann)
            # any shape at all
            return ir.Array([ir.Spread(scope.fresh_name())]), False
        raise FrontendError(f"unsupported annotation `{ast.unparse(ann)}` on {what}", ann)

    arg = ann.slice
    if name in ("tuple", "Tuple"):
        if binding:
            raise FrontendError(f"tuples are only supported as return types, on {what}", ann)
        elts = arg.elts if isinstance(arg, ast.Tuple) else [arg]
        if any(isinstance(e, ast.Constant) and e.value is Ellipsis for e in elts):
            raise FrontendError(f"only fixed-length tuples are supported, on {what}", ann)
        return ir.TupleType([typ_of_ast(e, scope, binding, what)[0] for e in elts]), False
    if name == "Literal":
        value = literal_int(arg)
        if value is None:
            raise FrontendError(f"only int and bool Literals are supported, on {what}", ann)
        return ir.Literal(value), False
    if name in DTYPES:
        if not (
            isinstance(arg, ast.Tuple)
            and len(arg.elts) == 2
            and isinstance(arg.elts[1], ast.Constant)
            and isinstance(arg.elts[1].value, str)
        ):
            raise FrontendError(f'expected {name}[ArrayType, "shape"] on {what}', ann)
        return ir.Array(parse_shape(arg.elts[1].value, scope, binding)), False
    if scope.stub and name in ("Dim", "Shape"):
        if not (isinstance(arg, ast.Constant) and isinstance(arg.value, str)):
            raise FrontendError(f'expected {name}["..."] on {what}', ann)
        if name == "Shape":
            return ir.Array(parse_shape(arg.value, scope, binding)), True
        try:
            node = ast.parse(arg.value, mode="eval").body
        except SyntaxError:
            raise FrontendError(f"can't parse Dim[{arg.value!r}] on {what}", ann) from None
        return ir.IntExpr(arith(node, scope)), False
    raise FrontendError(f"unsupported annotation `{ast.unparse(ann)}` on {what}", ann)


def literal_int(node: ast.expr) -> int | None:
    """The value of an int or bool literal (bools are 0 and 1), if node is one."""
    if isinstance(node, ast.Constant) and isinstance(node.value, (int, bool)):
        return int(node.value)
    if isinstance(node, ast.UnaryOp) and isinstance(node.op, ast.USub) and is_int(node.operand):
        return -node.operand.value
    return None


def default_term(node: ast.expr) -> Json | None:
    """The term a default value stands for, or None for a None default (the
    argument must then be given)."""
    if isinstance(node, ast.Constant) and node.value is None:
        return None
    value = literal_int(node)
    if value is not None:
        return ir.Lit(value)
    if isinstance(node, ast.Constant) and isinstance(node.value, float):
        return ir.Scalar()
    raise FrontendError(f"unsupported default value `{ast.unparse(node)}`", node)


def entry_names(e: Json) -> Iterable[str]:
    """The dim names an entry or constraint mentions."""
    if isinstance(e, list) and e:
        if e[0] == "Id":
            yield e[1]
        else:
            for x in e[1:]:
                yield from entry_names(x)


def constraints(test: ast.expr, scope: Scope) -> list[Json]:
    """The constraints an assert states, e.g. assert prod(A) == prod(B)."""
    if isinstance(test, ast.BoolOp) and isinstance(test.op, ast.And):
        return [c for v in test.values for c in constraints(v, scope)]
    if not isinstance(test, ast.Compare):
        raise FrontendError(f"unsupported assert `{ast.unparse(test)}`", test)
    out = []
    left = test.left
    for op, right in zip(test.ops, test.comparators):
        a, b = arith(left, scope), arith(right, scope)
        if isinstance(op, ast.Eq):
            out.append(["Eq", a, b])
        elif isinstance(op, ast.LtE):
            out.append(["Le", a, b])
        elif isinstance(op, ast.Lt):
            out.append(["Lt", a, b])
        elif isinstance(op, ast.GtE):
            out.append(["Le", b, a])
        elif isinstance(op, ast.Gt):
            out.append(["Lt", b, a])
        else:
            raise FrontendError(f"unsupported comparison in `{ast.unparse(test)}`", test)
        left = right
    return out


def build_signature(fn: ast.FunctionDef, stub: bool) -> tuple[Overload, Scope]:
    """The signature of fn, and the scope its body's annotations share."""
    a = fn.args
    if a.kwarg is not None:
        raise FrontendError("**kwargs isn't supported", fn)
    int_params: set[str] = set()
    scope = Scope(stub)
    taken = set() if stub else names_in(shape_strings(fn))

    def ir_name(name: str, typ: Json) -> str:
        # user functions keep shape names and parameter names apart, except
        # that a dim named after an int parameter is its value
        if typ[0] in ("Int", "Literal"):
            return name
        while name in taken:
            name += "'"
        return name

    positional = a.posonlyargs + a.args
    defaults: list[ast.expr | None] = [None] * (len(positional) - len(a.defaults)) + list(
        a.defaults
    )
    specs = [
        (arg, "positional" if i < len(a.posonlyargs) else "normal", d)
        for i, (arg, d) in enumerate(zip(positional, defaults))
    ]
    if a.vararg is not None:
        specs.append((a.vararg, "varargs", None))
    specs += [(arg, "keyword", d) for arg, d in zip(a.kwonlyargs, a.kw_defaults)]

    params: list[Param] = []
    ir_params: list[Json] = []
    for arg, kind, default in specs:
        what = f"parameter {arg.arg}"
        try:
            typ, shape = typ_of(arg.annotation, scope, True, what)
        except FrontendError as e:
            e.line = e.line or arg.lineno
            raise
        if kind == "varargs" and not shape:
            raise FrontendError("*args is only supported in stubs, as *shape: Shape[...]", arg)
        name = ir_name(arg.arg, typ)
        if typ[0] in ("Int", "Literal", "IntExpr"):
            int_params.add(name)
        params.append(
            Param(
                name=arg.arg,
                ir_name=name,
                kind=kind,
                default=None if default is None else default_term(default),
                shape=shape,
            )
        )
        ir_params.append([name, typ])
    scope.int_params = int_params

    ret, _ = typ_of(fn.returns, scope, False, "the return type")
    # jaxtyping: a name only the return type mentions is existential
    exists: list[str] = []
    for e in (e for a in arrays(ret) for e in a[1]):
        if (
            e[0] == "Id"
            and e[1] not in scope.dims
            and e[1] not in int_params
            and e[1] not in exists
        ):
            exists.append(e[1])

    requires: list[Json] = []
    ensures: list[Json] = []
    if stub:
        for s in fn.body:
            if isinstance(s, ast.Expr) and isinstance(s.value, ast.Constant):
                continue  # ... or a docstring
            if isinstance(s, ast.Pass):
                continue
            if not isinstance(s, ast.Assert):
                raise FrontendError("a stub's body may only contain asserts", s)
            try:
                cs = constraints(s.test, scope)
            except ShapeError as e:
                raise FrontendError(str(e), s) from None
            for c in cs:
                mentions_exists = any(x in exists for x in entry_names(c))
                (ensures if mentions_exists else requires).append(c)

    sig = ir.signature(ir_params, ret, requires, exists, ensures)
    return Overload(sig=sig, params=params), scope


def arrays(typ: Json) -> Iterable[Json]:
    """The array types in a type, including a tuple's elements."""
    if typ[0] == "Array":
        yield typ
    elif typ[0] == "Tuple":
        for t in typ[1]:
            yield from arrays(t)


def names_in(strings: Iterable[str]) -> set[str]:
    return {m for s in strings for m in re.findall(r"[A-Za-z_][A-Za-z0-9_]*", s)}
