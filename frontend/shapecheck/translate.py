"""Translating a Python module into a checker program.

Every top-level function with an annotation is checked, and so is every
annotated method of an nn.Module subclass. A body must be straight-line code:
assignments (plain, annotated, or unpacking a tuple), asserts (ignored), and a
return. Calls resolve to user functions (in any order, through their
signatures) or stubs; operators desugar to the stub module `operator` (x @ w
is operator.matmul), methods and properties to the stub classes (x.sum(-1) is
torch.Tensor.sum(x, -1)). Keyword and default arguments are bound in the
frontend, so the checker only sees positional arguments.

Modules are lowered away. An instance is known by its instance dims, the int
arguments its __init__ was called with, and a method is a function that takes
them first. In a method of Encoder, `self.w = nn.Linear(d_model, d_ff)` in
__init__ makes `self.w(x)` the call torch.nn.Linear.forward(d_model, d_ff, x),
where d_model is the method's own instance dim. So an attribute's type comes
from the one assignment to it in __init__.
"""

from __future__ import annotations

import ast
from collections.abc import Callable, Sequence
from dataclasses import dataclass, field
from pathlib import Path
from typing import Union

from . import ir
from .ir import Json
from .shapes import Scope
from .signatures import (
    FrontendError,
    Overload,
    Param,
    build_signature,
    instance_ints,
    typ_of,
)

# ---- stubs ----


@dataclass
class Callee:
    name: str  # qualified, e.g. torch.Tensor.sum
    overloads: list[Overload]


@dataclass
class ModuleClass:
    """A class whose instances are modules: a stub class such as
    torch.nn.Linear, or a user's nn.Module subclass. Its instance dims are
    the int parameters of its __init__."""

    name: str  # qualified for stubs, e.g. torch.nn.Linear
    ints: list[str]
    user: bool = False
    # user classes: `self.f = value` at the top level of __init__, or None if
    # f is assigned more than once or elsewhere
    fields: dict[str, ast.expr | None] = field(default_factory=dict)
    self_name: str = "self"  # __init__'s name for self

    @property
    def init(self) -> str:
        return f"{self.name}.__init__"

    def instances(self) -> list[Json]:
        """For a method's signature: its leading int parameters are an
        instance of this class, so they satisfy __init__'s requires."""
        return [{"init": self.init, "ints": [[n, n] for n in self.ints]}]


@dataclass
class Instance:
    """A module value: its class, and a term for each instance dim."""

    cls: ModuleClass
    ints: list[Json]


@dataclass
class Stubs:
    functions: dict[str, Callee] = field(default_factory=dict)
    modules: set[str] = field(default_factory=set)
    methods: dict[str, list[str]] = field(default_factory=dict)
    properties: dict[str, list[str]] = field(default_factory=dict)
    classes: dict[str, ModuleClass] = field(default_factory=dict)

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
        elif isinstance(node, ast.ClassDef) and any(last(b) == "Module" for b in node.bases):
            load_module_class(stubs, f"{module}.{node.name}", node)
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


def load_module_class(stubs: Stubs, qualname: str, node: ast.ClassDef) -> None:
    """A stub module class. Its methods aren't in the method tables: they're
    only called on its instances."""
    methods = [m for m in node.body if isinstance(m, ast.FunctionDef)]
    inits = [m for m in methods if m.name == "__init__"]
    if len(inits) > 1:
        raise FrontendError("a module class's __init__ can't be overloaded", inits[1])
    cls = ModuleClass(qualname, instance_ints(inits[0] if inits else None))
    stubs.classes[qualname] = cls
    for m in methods:
        if m.name == "__init__":
            ov = build_signature(m, stub=True, init=True)[0]
        else:
            ov = build_signature(m, stub=True, instance=cls.ints)[0]
            if cls.ints:
                ov.sig["instances"] = cls.instances()
        stubs.add(f"{qualname}.{m.name}", ov)


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
    """A top-level function or a method to check."""

    node: ast.FunctionDef
    cls: ModuleClass | None = None  # a method's class
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


class CycleError(FrontendError):
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
        self.classes: dict[str, ModuleClass] = {}  # user classes
        self.locals: dict[str, str] = {}  # Python name -> IR name
        self.cls: ModuleClass | None = None  # the class of the method translated
        self.self_name: str | None = None  # its name for self
        self.in_init = False
        self.in_instance = False  # translating a field's value (see as_instance)
        self.returns_none = False
        # a field's value, as terms over its class's instance dims
        # None while being computed, to catch attributes defined by each other
        self.field_terms: dict[tuple[str, str], list[Json] | None] = {}

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
            elif isinstance(node, ast.ClassDef):
                self.class_def(node)
            elif isinstance(node, ast.AsyncFunctionDef):
                self.notes.append(
                    Error(
                        node.lineno, None, f"skipped `{node.name}`: async functions aren't checked"
                    )
                )

        for name, f in self.functions.items():
            try:
                if f.cls is None:
                    f.overload, f.scope = build_signature(f.node, stub=False)
                elif f.node.name == "__init__":
                    f.overload, f.scope = build_signature(f.node, stub=False, init=True)
                else:
                    f.overload, f.scope = build_signature(f.node, stub=False, instance=f.cls.ints)
                    if f.cls.ints:
                        f.overload.sig["instances"] = f.cls.instances()
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

    # ---- classes ----

    def class_def(self, node: ast.ClassDef) -> None:
        """An nn.Module subclass: its annotated methods are checked, and its
        __init__ gives its instance dims and its attributes."""
        if [self.qualname(b) for b in node.bases] != ["torch.nn.Module"] or node.keywords:
            self.notes.append(
                Error(
                    node.lineno,
                    None,
                    f"skipped `{node.name}`: only classes that subclass nn.Module are checked",
                )
            )
            return
        methods = [m for m in node.body if isinstance(m, ast.FunctionDef)]
        init = next((m for m in reversed(methods) if m.name == "__init__"), None)
        cls = ModuleClass(node.name, instance_ints(init), user=True)
        if init is not None:
            cls.self_name = init.args.args[0].arg if init.args.args else "self"
            cls.fields = fields_of(init, cls.self_name)
        self.classes[node.name] = cls
        for m in methods:
            key = f"{node.name}.{m.name}"
            if m.decorator_list:
                if annotated(m):
                    self.errors.append(
                        Error(m.lineno, key, "decorated methods aren't supported yet")
                    )
                continue
            # an __init__ with nothing to annotate is checked too
            nothing = (
                m.name == "__init__"
                and len(m.args.args) == 1
                and not (m.args.posonlyargs or m.args.kwonlyargs or m.args.vararg or m.args.kwarg)
            )
            if annotated(m) or nothing:
                self.functions[key] = Function(m, cls)
                self.unannotated.discard(key)
            else:
                self.functions.pop(key, None)
                self.unannotated.add(key)

    def class_named(self, e: ast.expr) -> ModuleClass | None:
        """The module class an expression like nn.Linear or Encoder names."""
        if isinstance(e, ast.Name) and e.id not in self.locals and e.id in self.classes:
            return self.classes[e.id]
        q = self.qualname(e)
        return None if q is None else self.stubs.classes.get(q)

    def instance(self, e: ast.expr) -> Instance | None:
        """The module e is, if it's self or a module attribute of one."""
        if isinstance(e, ast.Name):
            if self.cls is None or e.id != self.self_name or e.id in self.locals:
                return None
            return Instance(self.cls, [ir.Var(n) for n in self.cls.ints])
        if isinstance(e, ast.Attribute):
            base = self.instance(e.value)
            if base is None or not base.cls.user:
                return None
            value = base.cls.fields.get(e.attr)
            if not (isinstance(value, ast.Call) and self.in_class(base.cls, value.func)):
                return None
            return self.field_instance(base, e.attr, e)
        return None

    def in_class(self, cls: ModuleClass, e: ast.expr) -> ModuleClass | None:
        """class_named, where the names are those of cls's __init__."""
        return self.as_instance(cls, lambda: self.class_named(e))

    def as_instance(self, cls: ModuleClass, f):
        """Run f where the names are those of cls's __init__, standing for an
        instance's dims: its int parameters, and self."""
        saved = (self.locals, self.terms, self.cls, self.self_name, self.in_init, self.in_instance)
        self.locals = {n: n for n in cls.ints}
        self.terms = {}
        self.cls, self.self_name, self.in_init, self.in_instance = cls, cls.self_name, False, True
        try:
            return f()
        finally:
            (
                self.locals,
                self.terms,
                self.cls,
                self.self_name,
                self.in_init,
                self.in_instance,
            ) = saved

    def field(self, cls: ModuleClass, name: str, e: ast.AST) -> ast.expr:
        """The value __init__ assigns to self.name."""
        if name not in cls.fields:
            raise FrontendError(f"`{cls.name}` has no attribute `{name}` assigned in __init__", e)
        value = cls.fields[name]
        if value is None:
            raise FrontendError(
                f"`self.{name}` must be assigned once, at the top level of "
                f"`{cls.name}.__init__`, for its methods to know its value",
                e,
            )
        return value

    def field_instance(self, base: Instance, name: str, e: ast.AST) -> Instance:
        """The module base.name: the arguments its constructor was called with
        in __init__, as terms over base's instance dims."""
        value = self.field(base.cls, name, e)
        assert isinstance(value, ast.Call)
        cls = self.in_class(base.cls, value.func)
        assert cls is not None

        def terms() -> list[Json]:
            params = self.init_params(cls, value)
            thunks = self.bind_thunks(params, list(value.args), keywords_of(value))
            by_name = dict(zip((p.name for p in params), thunks))
            return [by_name[n]() for n in cls.ints]

        ints = self.field_terms_of(
            base.cls, name, terms, e, f"the arguments that build `self.{name}`"
        )
        return Instance(cls, [substitute(t, base) for t in ints])

    def value_field(self, base: Instance, name: str, e: ast.AST) -> Json:
        """Any other attribute, e.g. self.d_k = d_model // h: the expression
        __init__ assigns it, evaluated again from base's instance dims. It
        has no other inputs, so it has the same shape (or int value)."""
        value = self.field(base.cls, name, e)
        (term,) = self.field_terms_of(
            base.cls, name, lambda: [self.term(value)], e, f"`self.{name}`"
        )
        return substitute(term, base)

    def field_terms_of(
        self, cls: ModuleClass, name: str, compute, e: ast.AST, what: str
    ) -> list[Json]:
        """compute() where the names are those of cls's __init__, once per
        attribute."""
        key = (cls.name, name)
        if key in self.field_terms:
            if self.field_terms[key] is None:
                raise CycleError(f"`self.{name}` is defined in terms of itself", e)
            return self.field_terms[key]
        self.field_terms[key] = None
        try:
            terms = self.as_instance(cls, compute)
        except CycleError as err:
            del self.field_terms[key]
            raise CycleError(err.message, e) from None  # reported where it's used
        except (FrontendError, BindError) as err:
            del self.field_terms[key]
            message = err.message if isinstance(err, FrontendError) else str(err)
            raise FrontendError(
                f"{what} can't be computed outside `{cls.name}.__init__`: {message}", e
            ) from None
        self.field_terms[key] = terms
        return terms

    def init_params(self, cls: ModuleClass, e: ast.AST) -> list[Param]:
        """The parameters of cls's constructor, not counting self."""
        if cls.user:
            f = self.functions.get(cls.init)
            if f is None:
                if cls.init in self.unannotated:
                    raise FrontendError(f"`{cls.init}` has no annotations", e)
                return []
            if f.overload is None:
                raise FrontendError(f"`{cls.init}`'s signature has errors", e)
            return f.overload.params
        callee = self.stubs.functions.get(cls.init)
        return [] if callee is None else callee.overloads[0].params

    def construct(self, cls: ModuleClass, e: ast.Call) -> Json:
        """A call to cls's constructor, which returns None; the frontend
        keeps track of the instance."""
        args, keywords = list(e.args), keywords_of(e)
        if cls.user:
            params = self.init_params(cls, e)
            if cls.init not in self.functions:
                if args or keywords:
                    raise FrontendError(f"`{cls.name}` takes no arguments", e)
                return ir.Tuple([])
            try:
                return ir.Call(cls.init, self.bind(params, args, keywords))
            except BindError as err:
                raise FrontendError(
                    f"`{cls.name}` doesn't take these arguments: {err}", e
                ) from None
        callee = self.stubs.functions.get(cls.init)
        if callee is None:
            if args or keywords:
                raise FrontendError(f"no stub for `{cls.init}`", e)
            return ir.Tuple([])
        return self.stub_call(callee, args, keywords, e)

    def method_call(
        self, inst: Instance, name: str, args: list[Arg], keywords: dict[str, ast.expr], e: ast.AST
    ) -> Json:
        """A call to an instance's method, which takes its dims first."""
        key = f"{inst.cls.name}.{name}"
        if name == "__init__":
            raise FrontendError("calling `__init__` isn't supported", e)
        if inst.cls.user:
            f = self.functions.get(key)
            if name in inst.cls.fields:
                self.field(inst.cls, name, e)  # it must be assigned once
                raise FrontendError(f"`self.{name}` isn't a module, so it can't be called", e)
            if f is None:
                if key in self.unannotated:
                    raise FrontendError(
                        f"`{key}` has no annotations, so calls to it can't be checked", e
                    )
                raise FrontendError(f"`{inst.cls.name}` has no method `{name}`", e)
            if f.overload is None:
                raise FrontendError(f"`{key}`'s signature has errors", e)
            try:
                return ir.Call(key, inst.ints + self.bind(f.overload.params, args, keywords))
            except BindError as err:
                raise FrontendError(f"`{key}` doesn't take these arguments: {err}", e) from None
        callee = self.stubs.functions.get(key)
        if callee is None:
            raise FrontendError(f"no stub for `{key}`", e)
        # its invariant is the constructor's requires
        init = self.stubs.functions.get(inst.cls.init)
        if init is not None:
            self.env[init.name] = [ov.sig for ov in init.overloads]
        return self.stub_call(callee, args, keywords, e, prefix=inst.ints)

    # ---- statements ----

    def body(self, f: Function) -> list[Json]:
        assert f.overload is not None and f.scope is not None
        self.locals = {p.name: p.ir_name for p in f.overload.params}
        self.terms = {}
        self.scope = f.scope
        self.cls = f.cls
        self.in_init = f.cls is not None and f.node.name == "__init__"
        self.self_name = f.node.args.args[0].arg if f.cls is not None else None
        self.returns_none = f.overload.sig["ret"] == ir.NoneType()
        out: list[Json] = []
        stmts = list(f.node.body)
        if stmts and is_docstring(stmts[0]):
            stmts.pop(0)
        for s in stmts:
            if isinstance(s, (ast.Assert, ast.Pass)):
                continue  # dropping a runtime check is sound
            if self.in_init and is_super_init(s):
                continue  # nn.Module's __init__
            out.append(ir.At(s.lineno, self.text(s), self.stmt(s)))
            if isinstance(s, ast.Return):
                break
        # falling off the end returns None
        if self.returns_none and not (out and out[-1][3][0] == "Return"):
            out.append(ir.Return(ir.Tuple([])))
        return out

    def stmt(self, s: ast.stmt) -> Json:
        if isinstance(s, ast.Assign):
            target = s.targets[0] if len(s.targets) == 1 else None
            if isinstance(target, ast.Tuple) and all(isinstance(x, ast.Name) for x in target.elts):
                t = self.term(s.value)
                return ir.Unpack([self.assign(x.id) for x in target.elts], t)
            if isinstance(target, ast.Attribute) and self.instance(target.value) is not None:
                return self.set_attribute(target, s.value)
            if not isinstance(target, ast.Name):
                raise FrontendError(
                    "only assignments to a name, or unpacking into names, are supported", s
                )
            t = self.term(s.value)
            return ir.Let(self.assign(target.id), t)
        if isinstance(s, ast.AnnAssign):
            if (
                isinstance(s.target, ast.Attribute)
                and self.instance(s.target.value) is not None
                and s.value is not None
            ):
                return self.set_attribute(s.target, s.value)
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
            if s.value is None or (isinstance(s.value, ast.Constant) and s.value.value is None):
                if not self.returns_none:
                    raise FrontendError("a checked function must return a value", s)
                return ir.Return(ir.Tuple([]))
            return ir.Return(self.term(s.value))
        kind = {
            ast.If: "`if`", ast.For: "`for`", ast.While: "`while`", ast.With: "`with`",
            ast.Try: "`try`", ast.FunctionDef: "a nested function",
            ast.Expr: "an expression statement",
        }.get(type(s), "this statement")  # fmt: skip
        raise FrontendError(f"{kind} isn't supported yet: `{self.text(s)}`", s)

    def assign(self, name: str) -> str:
        if self.cls is not None and name in self.cls.ints:
            if self.in_init:
                raise FrontendError(
                    f"`__init__` can't reassign `{name}`: it's one of the instance's dims, "
                    "which its methods refer to"
                )
            # the IR name is the method's instance dim
            ir_name = name
            while ir_name in self.cls.ints:
                ir_name += "'"
            self.locals[name] = ir_name
            return ir_name
        self.locals[name] = name
        return name

    def set_attribute(self, target: ast.Attribute, value: ast.expr) -> Json:
        """self.f = value, in __init__. A module attribute is built by
        calling its constructor, and later uses of self.f resolve through
        this assignment (see field_instance)."""
        if not self.in_init:
            raise FrontendError("attributes can only be assigned in `__init__`", target)
        if not isinstance(target.value, ast.Name):
            raise FrontendError(
                f"assigning to an attribute of a module (`{ast.unparse(target)}`) "
                "isn't supported yet",
                target,
            )
        cls = self.class_named(value.func) if isinstance(value, ast.Call) else None
        t = self.construct(cls, value) if cls is not None else self.term(value)
        # later statements read self.f from the class's fields, not this name
        return ir.Let(f"{target.value.id}.{target.attr}", t)

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
            if self.instance(e) is not None:
                raise FrontendError(
                    f"`{e.id}` can only be used for its attributes and methods here", e
                )
            if self.in_instance:
                raise FrontendError(f"`{e.id}` isn't an int parameter of `__init__`", e)
            if e.id in self.functions or e.id in self.aliases or e.id in self.classes:
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
        keywords = keywords_of(e)
        args: list[Arg] = list(e.args)
        cls = self.class_named(f)
        if cls is not None:
            raise FrontendError(
                "a module can only be created in `__init__`, as "
                f"`self.name = {ast.unparse(f)}(...)`",
                e,
            )
        inst = self.instance(f)
        if inst is not None:
            return self.method_call(inst, "forward", args, keywords, e)
        inst = self.instance(f.value) if isinstance(f, ast.Attribute) else None
        if inst is not None:
            return self.method_call(inst, f.attr, args, keywords, e)
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
        base = self.instance(e.value)
        if base is not None:
            if not base.cls.user:
                raise FrontendError(f"attributes of `{base.cls.name}` aren't supported yet", e)
            key = f"{base.cls.name}.{e.attr}"
            if e.attr not in base.cls.fields and (key in self.functions or key in self.unannotated):
                raise FrontendError(f"`{ast.unparse(e)}` is a method; call it", e)
            if self.instance(e) is not None:
                raise FrontendError(
                    f"`{ast.unparse(e)}` is a module; only calls to it are supported", e
                )
            return self.value_field(base, e.attr, e)
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
        self,
        callee: Callee,
        args: list[Arg],
        keywords: dict[str, ast.expr],
        e: ast.AST,
        prefix: list[Json] = (),
    ) -> Json:
        """A call to the overloads whose parameters accept the arguments. The
        checker then picks among them by type. prefix is a method's instance
        dims, which come before the arguments."""
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
        return ir.Call(key, list(prefix) + bound[0][1])

    def bind(
        self, params: list[Param], args: list[Arg], keywords: dict[str, ast.expr]
    ) -> list[Json]:
        """Python's argument binding, giving one term per parameter."""
        return [thunk() for thunk in self.bind_thunks(params, args, keywords)]

    def bind_thunks(
        self, params: list[Param], args: list[Arg], keywords: dict[str, ast.expr]
    ) -> list[Callable[[], Json]]:
        """bind, with each term translated only when asked for: a module
        attribute's instance dims come from some of its constructor's
        arguments (see field_instance)."""
        bound: dict[int, Callable[[], Json]] = {}
        i = 0
        for k, p in enumerate(params):
            if p.kind == "keyword":
                break
            if p.kind == "varargs":
                rest, i = args[i:], len(args)
                if len(rest) == 1 and isinstance(rest[0], ast.Tuple):
                    rest = list(rest[0].elts)  # reshape((n, d)) as well as reshape(n, d)
                bound[k] = lambda rest=rest: ir.Shape([self.arg_term(a, None) for a in rest])
                continue
            if i < len(args):
                bound[k] = self.arg_thunk(args[i], p)
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
            bound[matches[0]] = self.arg_thunk(value, params[matches[0]])
        for k, p in enumerate(params):
            if k not in bound:
                if p.default is None:
                    raise BindError(f"missing argument `{p.name}`")
                bound[k] = lambda d=p.default: d
        return [bound[k] for k in range(len(params))]

    def arg_thunk(self, a: Arg, p: Param) -> Callable[[], Json]:
        # a tuple for a shape parameter is a binding error, found eagerly
        if not isinstance(a, tuple) and p.shape and not isinstance(a, ast.Tuple):
            raise BindError(f"`{p.name}` takes a tuple of ints")
        if isinstance(a, ast.Tuple) and not p.shape:
            raise BindError("a tuple isn't expected here")
        return lambda: self.arg_term(a, p)

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


def keywords_of(e: ast.Call) -> dict[str, ast.expr]:
    return {k.arg: k.value for k in e.keywords if k.arg is not None}


def substitute(t: Json, inst: Instance) -> Json:
    """A term over inst's class's instance dims, over inst's terms instead."""
    mapping = dict(zip(inst.cls.ints, inst.ints))

    def go(t: Json) -> Json:
        if isinstance(t, list) and len(t) == 2 and t[0] == "Var" and isinstance(t[1], str):
            return mapping.get(t[1], t)
        if isinstance(t, list):
            return [go(x) for x in t]
        return t

    return go(t)


def fields_of(init: ast.FunctionDef, self_name: str) -> dict[str, ast.expr | None]:
    """The attributes __init__ assigns: self.f = value at its top level. An
    attribute assigned more than once, or anywhere else, maps to None."""

    def attribute(t: ast.expr) -> str | None:
        if (
            isinstance(t, ast.Attribute)
            and isinstance(t.value, ast.Name)
            and t.value.id == self_name
        ):
            return t.attr
        return None

    fields: dict[str, ast.expr | None] = {}
    top: set[int] = set()
    for s in init.body:
        target = value = None
        if isinstance(s, ast.Assign) and len(s.targets) == 1:
            target, value = s.targets[0], s.value
        elif isinstance(s, ast.AnnAssign):
            target, value = s.target, s.value
        name = None if target is None else attribute(target)
        if name is not None and value is not None:
            top.add(id(target))
            fields[name] = value if name not in fields else None
    for node in ast.walk(init):
        if isinstance(node, ast.Attribute) and isinstance(node.ctx, (ast.Store, ast.Del)):
            name = attribute(node)
            if name is not None and id(node) not in top:
                fields[name] = None
    return fields


def is_super_init(s: ast.stmt) -> bool:
    """super().__init__(), with no arguments."""
    return (
        isinstance(s, ast.Expr)
        and isinstance(s.value, ast.Call)
        and not s.value.args
        and not s.value.keywords
        and isinstance(s.value.func, ast.Attribute)
        and s.value.func.attr == "__init__"
        and isinstance(s.value.func.value, ast.Call)
        and isinstance(s.value.func.value.func, ast.Name)
        and s.value.func.value.func.id == "super"
        and not s.value.func.value.args
    )


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
