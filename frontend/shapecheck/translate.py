"""Translating a Python module into a checker program.

Every top-level function with an annotation is checked; classes are skipped
with a note. A body must be straight-line code: assignments (plain, annotated,
or unpacking a tuple), asserts (ignored), and a return. Calls resolve to user functions
(in any order, through their signatures) or stubs;
operators desugar to the stub module `operator` (x @ w is operator.matmul),
methods and properties to the stub classes (x.sum(-1) is
torch.Tensor.sum(x, -1)). Keyword and default arguments are bound in the
frontend, so the checker only sees positional arguments.
"""

from __future__ import annotations

import ast
from collections.abc import Sequence
from dataclasses import dataclass, field
from pathlib import Path
from typing import Union

from . import ir
from .ir import Json
from .shapes import Scope
from .signatures import FrontendError, Overload, Param, build_signature, typ_of

# ---- stubs ----


@dataclass
class Callee:
    name: str  # qualified, e.g. torch.Tensor.sum
    overloads: list[Overload]


@dataclass
class Stubs:
    functions: dict[str, Callee] = field(default_factory=dict)
    modules: set[str] = field(default_factory=set)
    methods: dict[str, list[str]] = field(default_factory=dict)
    properties: dict[str, list[str]] = field(default_factory=dict)

    def add(self, qualname: str, ov: Overload) -> None:
        self.functions.setdefault(qualname, Callee(qualname, [])).overloads.append(ov)


def load_stubs(dirs: Sequence[Path]) -> Stubs:
    """Stubs from .pyi files: stubs/torch/nn/functional.pyi is the module
    torch.nn.functional. Repeated definitions of a name are its overloads."""
    stubs = Stubs()
    for d in dirs:
        for path in sorted(Path(d).rglob("*.pyi")):
            parts = list(path.relative_to(d).with_suffix("").parts)
            if parts[-1] == "__init__":
                parts.pop()
            module = ".".join(parts)
            for i in range(1, len(parts) + 1):
                stubs.modules.add(".".join(parts[:i]))
            try:
                load_stub_module(stubs, module, ast.parse(path.read_text(), str(path)))
            except FrontendError as e:
                raise FrontendError(f"{path}:{e.line}: {e.message}") from None
    return stubs


def load_stub_module(stubs: Stubs, module: str, tree: ast.Module) -> None:
    for node in tree.body:
        if isinstance(node, ast.FunctionDef):
            stubs.add(f"{module}.{node.name}", build_signature(node, stub=True)[0])
        elif isinstance(node, ast.ClassDef):
            for m in node.body:
                if not isinstance(m, ast.FunctionDef):
                    continue
                qualname = f"{module}.{node.name}.{m.name}"
                is_property = any(last(d) == "property" for d in m.decorator_list)
                table = stubs.properties if is_property else stubs.methods
                names = table.setdefault(m.name, [])
                if qualname not in names:
                    names.append(qualname)
                stubs.add(qualname, build_signature(m, stub=True)[0])


def last(node: ast.expr) -> str | None:
    if isinstance(node, ast.Name):
        return node.id
    if isinstance(node, ast.Attribute):
        return node.attr
    return None


# ---- programs ----

OPERATORS = {
    ast.Add: "add", ast.Sub: "sub", ast.Mult: "mul", ast.Div: "truediv",
    ast.FloorDiv: "floordiv", ast.MatMult: "matmul", ast.Pow: "pow",
    ast.BitAnd: "and_", ast.BitOr: "or_", ast.BitXor: "xor",
}  # fmt: skip
OP_TEXT = {
    ast.Add: "+", ast.Sub: "-", ast.Mult: "*", ast.Div: "/", ast.FloorDiv: "//",
    ast.MatMult: "@", ast.Pow: "**", ast.Mod: "%", ast.BitAnd: "&", ast.BitOr: "|",
    ast.BitXor: "^",
}  # fmt: skip
COMPARISONS = {
    ast.Lt: "lt", ast.LtE: "le", ast.Gt: "gt", ast.GtE: "ge", ast.Eq: "eq",
    ast.NotEq: "ne",
}  # fmt: skip
SYMBOLS = {
    "add": "+", "sub": "-", "mul": "*", "truediv": "/", "floordiv": "//",
    "matmul": "@", "pow": "**", "neg": "unary -", "lt": "<", "le": "<=",
    "gt": ">", "ge": ">=", "eq": "==", "ne": "!=", "and_": "&", "or_": "|",
    "xor": "^", "invert": "~",
}  # fmt: skip


@dataclass
class Function:
    """A top-level function to check."""

    node: ast.FunctionDef
    overload: Overload | None = None  # None if its signature has errors
    scope: Scope | None = None
    body: list[Json] | None = None  # None if its body has errors


@dataclass
class Error:
    line: int
    function: str | None
    message: str


class BindError(Exception):
    pass


# an argument: an expression, or a term already translated (a receiver)
Arg = Union[ast.expr, tuple[str, Json]]


class Translator:
    def __init__(self, stubs: Stubs, source: str):
        self.stubs = stubs
        self.source = source
        self.aliases: dict[str, str] = {}  # local name -> qualified name
        self.functions: dict[str, Function] = {}
        self.unannotated: set[str] = set()
        self.env: dict[str, list[Json]] = {}  # the stub callees used
        self.errors: list[Error] = []
        self.notes: list[Error] = []
        self.terms: dict[int, Json] = {}

    def translate(self, tree: ast.Module) -> Json:
        for node in tree.body:
            if isinstance(node, ast.Import):
                for a in node.names:
                    if a.asname:
                        self.aliases[a.asname] = a.name
                    else:
                        root = a.name.split(".")[0]
                        self.aliases[root] = root
            elif isinstance(node, ast.ImportFrom) and node.module and not node.level:
                for a in node.names:
                    self.aliases[a.asname or a.name] = f"{node.module}.{a.name}"
            elif isinstance(node, ast.FunctionDef):
                if annotated(node):
                    self.functions[node.name] = Function(node)
                    self.unannotated.discard(node.name)
                else:
                    self.functions.pop(node.name, None)
                    self.unannotated.add(node.name)
            elif isinstance(node, (ast.ClassDef, ast.AsyncFunctionDef)):
                self.notes.append(
                    Error(
                        node.lineno,
                        None,
                        f"skipped `{node.name}`: only top-level functions are checked",
                    )
                )

        for name, f in self.functions.items():
            try:
                f.overload, f.scope = build_signature(f.node, stub=False)
            except FrontendError as e:
                self.errors.append(Error(e.line or f.node.lineno, name, e.message))
        for name, f in self.functions.items():
            if f.overload is None:
                continue
            try:
                f.body = self.body(f)
            except FrontendError as e:
                self.errors.append(Error(e.line or f.node.lineno, name, e.message))

        return {
            "env": [{"name": k, "overloads": v} for k, v in self.env.items()],
            "functions": [
                {"name": name, "sig": f.overload.sig, "body": f.body}
                for name, f in self.functions.items()
                if f.overload is not None
            ],
        }

    # ---- statements ----

    def body(self, f: Function) -> list[Json]:
        assert f.overload is not None and f.scope is not None
        self.locals: dict[str, str] = {p.name: p.ir_name for p in f.overload.params}
        self.scope = f.scope
        out: list[Json] = []
        stmts = list(f.node.body)
        if stmts and is_docstring(stmts[0]):
            stmts.pop(0)
        for s in stmts:
            if isinstance(s, (ast.Assert, ast.Pass)):
                continue  # dropping a runtime check is sound
            out.append(ir.At(s.lineno, self.text(s), self.stmt(s)))
            if isinstance(s, ast.Return):
                break
        return out

    def stmt(self, s: ast.stmt) -> Json:
        if isinstance(s, ast.Assign):
            target = s.targets[0] if len(s.targets) == 1 else None
            if isinstance(target, ast.Tuple) and all(isinstance(x, ast.Name) for x in target.elts):
                t = self.term(s.value)
                return ir.Unpack([self.assign(x.id) for x in target.elts], t)
            if not isinstance(target, ast.Name):
                raise FrontendError(
                    "only assignments to a name, or unpacking into names, are supported", s
                )
            t = self.term(s.value)
            return ir.Let(self.assign(target.id), t)
        if isinstance(s, ast.AnnAssign):
            if not isinstance(s.target, ast.Name) or s.value is None:
                raise FrontendError("only annotated assignments `x: T = ...` are supported", s)
            typ, _ = typ_of(s.annotation, self.scope, True, s.target.id)
            t = self.term(s.value)
            return ir.LetAnnot(self.assign(s.target.id), typ, t)
        if isinstance(s, ast.AugAssign):
            # in place, x keeps its shape, which the stubs can't state yet
            op = OP_TEXT.get(type(s.op), "?")
            target, value = ast.unparse(s.target), ast.unparse(s.value)
            raise FrontendError(
                "augmented assignment isn't supported yet; write "
                f"`{target} = {target} {op} {value}`",
                s,
            )
        if isinstance(s, ast.Return):
            if s.value is None:
                raise FrontendError("a checked function must return a value", s)
            return ir.Return(self.term(s.value))
        kind = {
            ast.If: "`if`", ast.For: "`for`", ast.While: "`while`", ast.With: "`with`",
            ast.Try: "`try`", ast.FunctionDef: "a nested function",
            ast.Expr: "an expression statement",
        }.get(type(s), "this statement")  # fmt: skip
        raise FrontendError(f"{kind} isn't supported yet: `{self.text(s)}`", s)

    def assign(self, name: str) -> str:
        self.locals[name] = name
        return name

    def text(self, node: ast.AST) -> str:
        src = ast.get_source_segment(self.source, node) or ast.unparse(node)
        lines = src.splitlines()
        first = " ".join(lines[0].split())
        return first + (" ..." if len(lines) > 1 else "")

    # ---- expressions ----

    def term(self, e: ast.expr) -> Json:
        # binding tries each overload, so an argument is translated once
        if id(e) not in self.terms:
            self.terms[id(e)] = self.term_of(e)
        return self.terms[id(e)]

    def term_of(self, e: ast.expr) -> Json:
        if isinstance(e, ast.Name):
            if e.id in self.locals:
                return ir.Var(self.locals[e.id])
            if e.id in self.functions or e.id in self.aliases:
                raise FrontendError(f"`{e.id}` is used as a value; only calls are supported", e)
            raise FrontendError(
                f"`{e.id}` isn't a local variable (module-level values aren't supported)", e
            )
        if isinstance(e, ast.Constant):
            if isinstance(e.value, (bool, int)):
                return ir.Lit(int(e.value))
            if isinstance(e.value, float):
                return ir.Scalar()
            raise FrontendError(f"unsupported constant `{ast.unparse(e)}`", e)
        if isinstance(e, ast.UnaryOp):
            if isinstance(e.op, ast.UAdd):
                return self.term(e.operand)
            if isinstance(e.op, ast.USub):
                if isinstance(e.operand, ast.Constant) and type(e.operand.value) is int:
                    return ir.Lit(-e.operand.value)
                if isinstance(e.operand, ast.Constant) and isinstance(e.operand.value, float):
                    return ir.Scalar()
                return self.operator("neg", [e.operand], e)
            if isinstance(e.op, ast.Invert):
                return self.operator("invert", [e.operand], e)
        if isinstance(e, ast.BinOp) and type(e.op) in OPERATORS:
            return self.operator(OPERATORS[type(e.op)], [e.left, e.right], e)
        if isinstance(e, ast.Compare) and len(e.ops) == 1 and type(e.ops[0]) in COMPARISONS:
            return self.operator(COMPARISONS[type(e.ops[0])], [e.left, e.comparators[0]], e)
        if isinstance(e, ast.Call):
            return self.call(e)
        if isinstance(e, ast.Attribute):
            return self.attribute(e)
        if isinstance(e, ast.Tuple):
            if any(isinstance(x, ast.Starred) for x in e.elts):
                raise FrontendError("starred items in tuples aren't supported", e)
            return ir.Tuple([self.term(x) for x in e.elts])
        if isinstance(e, ast.Subscript):
            if isinstance(e.value, ast.Attribute) and e.value.attr == "shape":
                raise FrontendError("`x.shape[i]` isn't supported yet", e)
            raise FrontendError("indexing isn't supported yet", e)
        raise FrontendError(f"unsupported expression `{ast.unparse(e)}`", e)

    def qualname(self, e: ast.expr) -> str | None:
        """The qualified name an expression like F.relu or torch.nn.functional
        refers to, if it's rooted at an import."""
        if isinstance(e, ast.Name):
            if e.id in self.locals:
                return None
            return self.aliases.get(e.id)
        if isinstance(e, ast.Attribute):
            base = self.qualname(e.value)
            return None if base is None else f"{base}.{e.attr}"
        return None

    def call(self, e: ast.Call) -> Json:
        f = e.func
        if any(isinstance(a, ast.Starred) for a in e.args) or any(
            k.arg is None for k in e.keywords
        ):
            raise FrontendError("*args and **kwargs in calls aren't supported", e)
        keywords = {k.arg: k.value for k in e.keywords if k.arg is not None}
        args: list[Arg] = list(e.args)
        if isinstance(f, ast.Name) and f.id not in self.locals and f.id in self.functions:
            user = self.functions[f.id]
            if user.overload is None:
                raise FrontendError(f"`{f.id}`'s signature has errors", e)
            try:
                return ir.Call(f.id, self.bind(user.overload.params, args, keywords))
            except BindError as err:
                raise FrontendError(f"`{f.id}` doesn't take these arguments: {err}", e) from None
        q = self.qualname(f)
        if q is not None:
            callee = self.stubs.functions.get(q)
            if callee is None:
                raise FrontendError(f"no stub for `{q}`", e)
            return self.stub_call(callee, args, keywords, e)
        if isinstance(f, ast.Name):
            if f.id in self.unannotated:
                raise FrontendError(
                    f"`{f.id}` has no annotations, so calls to it can't be checked", e
                )
            raise FrontendError(f"unknown function `{f.id}`", e)
        if isinstance(f, ast.Attribute):
            callee = self.member(self.stubs.methods, f, "method")
            return self.stub_call(callee, [("receiver", self.term(f.value))] + args, keywords, e)
        raise FrontendError(f"unsupported call `{ast.unparse(e)}`", e)

    def attribute(self, e: ast.Attribute) -> Json:
        if self.qualname(e) is not None:
            raise FrontendError(f"module attributes like `{ast.unparse(e)}` aren't supported", e)
        if e.attr == "shape":
            raise FrontendError("`x.shape` isn't supported yet", e)
        if e.attr not in self.stubs.properties and e.attr in self.stubs.methods:
            raise FrontendError(f"`.{e.attr}` is a method; call it", e)
        callee = self.member(self.stubs.properties, e, "attribute")
        return self.stub_call(callee, [("receiver", self.term(e.value))], {}, e)

    def member(self, table: dict[str, list[str]], e: ast.Attribute, what: str) -> Callee:
        names = table.get(e.attr, [])
        if not names:
            raise FrontendError(f"no stub for the {what} `.{e.attr}`", e)
        if len(names) > 1:
            raise FrontendError(f"the {what} `.{e.attr}` is ambiguous: " + ", ".join(names), e)
        return self.stubs.functions[names[0]]

    def operator(self, name: str, args: list[ast.expr], e: ast.AST) -> Json:
        callee = self.stubs.functions.get(f"operator.{name}")
        if callee is None:
            raise FrontendError(f"no stub for `{SYMBOLS[name]}` (operator.{name})", e)
        return self.stub_call(callee, list(args), {}, e)

    def stub_call(
        self, callee: Callee, args: list[Arg], keywords: dict[str, ast.expr], e: ast.AST
    ) -> Json:
        """A call to the overloads whose parameters accept the arguments. The
        checker then picks among them by type."""
        bound: list[tuple[int, list[Json]]] = []
        reasons = []
        for i, ov in enumerate(callee.overloads):
            try:
                bound.append((i, self.bind(ov.params, args, keywords)))
            except BindError as err:
                reasons.append((i, str(err)))
        if not bound:
            detail = (
                reasons[0][1]
                if len(reasons) == 1
                else "; ".join(f"overload {i + 1}: {r}" for i, r in reasons)
            )
            raise FrontendError(f"`{callee.name}` doesn't take these arguments: {detail}", e)
        if len({repr(b) for _, b in bound}) > 1:
            raise FrontendError(
                f"the arguments to `{callee.name}` bind differently to its overloads "
                + ", ".join(str(i + 1) for i, _ in bound),
                e,
            )
        indices = [i for i, _ in bound]
        key = callee.name
        if len(indices) < len(callee.overloads):
            key += " (overload{} {})".format(
                "s" if len(indices) > 1 else "", ", ".join(str(i + 1) for i in indices)
            )
        self.env[key] = [callee.overloads[i].sig for i in indices]
        return ir.Call(key, bound[0][1])

    def bind(
        self, params: list[Param], args: list[Arg], keywords: dict[str, ast.expr]
    ) -> list[Json]:
        """Python's argument binding, giving one term per parameter."""
        bound: dict[int, Json] = {}
        i = 0
        for k, p in enumerate(params):
            if p.kind == "keyword":
                break
            if p.kind == "varargs":
                rest, i = args[i:], len(args)
                if len(rest) == 1 and isinstance(rest[0], ast.Tuple):
                    rest = list(rest[0].elts)  # reshape((n, d)) as well as reshape(n, d)
                bound[k] = ir.Shape([self.arg_term(a, None) for a in rest])
                continue
            if i < len(args):
                bound[k] = self.arg_term(args[i], p)
                i += 1
        if i < len(args):
            raise BindError("too many positional arguments")
        for name, value in keywords.items():
            matches = [
                k for k, p in enumerate(params)
                if p.name == name and p.kind in ("normal", "keyword")
            ]  # fmt: skip
            if not matches:
                raise BindError(f"unexpected keyword argument `{name}`")
            if matches[0] in bound:
                raise BindError(f"`{name}` is given twice")
            bound[matches[0]] = self.arg_term(value, params[matches[0]])
        for k, p in enumerate(params):
            if k not in bound:
                if p.default is None:
                    raise BindError(f"missing argument `{p.name}`")
                bound[k] = p.default
        return [bound[k] for k in range(len(params))]

    def arg_term(self, a: Arg, p: Param | None) -> Json:
        if isinstance(a, tuple):
            return a[1]
        if p is not None and p.shape:
            if not isinstance(a, ast.Tuple):
                raise BindError(f"`{p.name}` takes a tuple of ints")
            return ir.Shape([self.term(x) for x in a.elts])
        if isinstance(a, ast.Tuple):
            raise BindError("a tuple isn't expected here")
        return self.term(a)


def annotated(fn: ast.FunctionDef) -> bool:
    a = fn.args
    args = a.posonlyargs + a.args + a.kwonlyargs + [a.vararg, a.kwarg]
    return fn.returns is not None or any(x is not None and x.annotation is not None for x in args)


def is_docstring(s: ast.stmt) -> bool:
    return (
        isinstance(s, ast.Expr)
        and isinstance(s.value, ast.Constant)
        and isinstance(s.value.value, str)
    )


def translate(source: str, stubs: Stubs, filename: str = "<string>") -> tuple[Json, Translator]:
    """The program for a module, and the translator with its errors."""
    t = Translator(stubs, source)
    try:
        tree = ast.parse(source, filename)
    except SyntaxError as e:
        t.errors.append(Error(e.lineno or 1, None, f"syntax error: {e.msg}"))
        return {"env": [], "functions": []}, t
    return t.translate(tree), t
