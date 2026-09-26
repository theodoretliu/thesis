"""Translating a Python module into a checker program.

Every top-level function with an annotation is checked, and so is every
annotated method of an nn.Module subclass. A body must be straight-line code:
assignments (plain, annotated, unpacking a tuple, or into a slice), asserts,
and a return. The exceptions are `if`s on whether a local is None, decided
statically, and loops over an nn.ModuleList, whose body is checked once.
Calls resolve to user functions (in any order, through their signatures) or
stubs; operators desugar to the stub module `operator` (x @ w is
operator.matmul), methods and properties to the stub classes (x.sum(-1) is
torch.Tensor.sum(x, -1)). Keyword and default arguments are bound in the
frontend, so the checker only sees positional arguments.

An Optional parameter is None or not at each call, and the frontend always
knows which. So a function with Optional parameters is checked once for each
choice of which are None, as separate checker functions (attention and
attention[mask=None]), and `if mask is not None:` is decided statically in
each. A call goes to the one its arguments select.

Modules are lowered away. An instance is known by its instance dims, the int
arguments its __init__ was called with, and a method is a function that takes
them first. In a method of Encoder, `self.w = nn.Linear(d_model, d_ff)` in
__init__ makes `self.w(x)` the call torch.nn.Linear.forward(d_model, d_ff, x),
where d_model is the method's own instance dim. So an attribute's type comes
from the one assignment to it in __init__, unless a class-level annotation
declares it.
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
    NONE,
    FrontendError,
    Overload,
    Param,
    attribute_signatures,
    attribute_type,
    build_signature,
    instance_ints,
    is_none,
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
    # attributes declared at class level, e.g. pe: Float[Tensor, "1 max_len d"]:
    # their IR types, over the instance dims
    attrs: dict[str, Json] = field(default_factory=dict)

    @property
    def init(self) -> str:
        return f"{self.name}.__init__"

    def getter(self, attr: str) -> str:
        """The checker function that reads a declared attribute."""
        return f"{self.name}.{attr}"

    def setter(self, attr: str) -> str:
        """The checker function that assigns a declared attribute."""
        return f"{self.name}.{attr} (assigned)"

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
    cls.attrs = declared_attributes(node, cls.ints, stub=True)
    stubs.classes[qualname] = cls
    for m in methods:
        if m.name == "__init__":
            ov = build_signature(m, stub=True, init=True)[0]
        else:
            ov = build_signature(m, stub=True, instance=cls.ints)[0]
            if cls.ints:
                ov.sig["instances"] = cls.instances()
        stubs.add(f"{qualname}.{m.name}", ov)


def declared_attributes(
    node: ast.ClassDef, ints: list[str], stub: bool, errors: list[FrontendError] | None = None
) -> dict[str, Json]:
    """The attributes a class body declares, e.g. pe: Float[Tensor, "1 max_len
    d"], with their types. A declaration with errors raises, or with errors
    given is left out and its error appended."""
    attrs = {}
    for s in node.body:
        if not (isinstance(s, ast.AnnAssign) and isinstance(s.target, ast.Name)):
            continue
        try:
            if s.value is not None:
                raise FrontendError(
                    f"class attributes with values aren't supported; assign `{s.target.id}` "
                    "in `__init__`",
                    s,
                )
            what = f"`{node.name}.{s.target.id}`"
            attrs[s.target.id] = attribute_type(s.annotation, ints, stub, what)
        except FrontendError as e:
            if errors is None:
                raise
            e.line = e.line or s.lineno
            errors.append(e)
    return attrs


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


# each Optional parameter doubles the checks, so there's a limit
MAX_OPTIONAL = 4

MODULE_LIST = "torch.nn.ModuleList"
MODULE_LIST_FORM = "`nn.ModuleList([Module(...) for _ in range(n)])`"


@dataclass
class Variant:
    """A function where some of its Optional parameters are None: a checker
    function of its own, named like attention[mask=None]."""

    nones: frozenset[str]
    name: str
    overload: Overload
    scope: Scope
    body: list[Json] | None = None  # None if its body has errors


@dataclass
class Function:
    """A top-level function or a method to check."""

    node: ast.FunctionDef
    cls: ModuleClass | None = None  # a method's class
    # the variant where nothing is None, whose params bind calls. None if its
    # signature has errors
    overload: Overload | None = None
    variants: list[Variant] = field(default_factory=list)

    def variant(self, nones: frozenset[str]) -> Variant:
        return next(v for v in self.variants if v.nones == nones)


@dataclass
class Error:
    line: int
    function: str | None
    message: str


class BindError(Exception):
    pass


class CycleError(FrontendError):
    pass


class Unstated(Exception):
    """Part of an assert the checker can't state, so it's dropped."""


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
        self.module_locals: dict[str, Instance] = {}  # module parameters
        self.nones: set[str] = set()  # locals that are None here
        self.after_loop: set[str] = set()  # names bound only inside a loop's body
        self.owners: dict[str, str] = {}  # a checker function's Python function
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
                f.variants = self.variants(name, f)
                f.overload = f.variants[0].overload
            except FrontendError as e:
                self.errors.append(Error(e.line or f.node.lineno, name, e.message))
            for v in f.variants:
                self.owners[v.name] = name
        for f in self.functions.values():
            reported = set()
            for v in f.variants:
                try:
                    v.body = self.body(f, v)
                except FrontendError as e:
                    line = e.line or f.node.lineno
                    # the same error in each variant is reported once
                    if (line, e.message) not in reported:
                        reported.add((line, e.message))
                        self.errors.append(Error(line, v.name, e.message))

        return {
            "env": [{"name": k, "overloads": v} for k, v in self.env.items()],
            "functions": [
                {"name": v.name, "sig": v.overload.sig, "body": v.body}
                for f in self.functions.values()
                for v in f.variants
            ],
        }

    def variants(self, name: str, f: Function) -> list[Variant]:
        """f's signature for each choice of which Optional parameters are
        None, starting with none of them."""

        def build(nones: frozenset[str]) -> tuple[Overload, Scope]:
            if f.cls is not None and f.node.name == "__init__":
                return build_signature(f.node, stub=False, init=True)
            instance = None if f.cls is None else f.cls.ints
            ov, scope = build_signature(
                f.node, stub=False, instance=instance, module_class=self.module_class, nones=nones
            )
            if f.cls is not None and f.cls.ints:
                ov.sig["instances"] = f.cls.instances() + ov.sig.get("instances", [])
            return ov, scope

        full = build(frozenset())
        for p in full[0].params:
            if p.module is not None:
                self.use_init(p.module)
        optional = [p.name for p in full[0].params if p.optional]
        if len(optional) > MAX_OPTIONAL:
            raise FrontendError(
                f"at most {MAX_OPTIONAL} parameters can be Optional: each one doubles "
                "the cases checked",
                f.node,
            )
        out = []
        for k in range(2 ** len(optional)):
            nones = frozenset(n for i, n in enumerate(optional) if k >> i & 1)
            ov, scope = full if not nones else build(nones)
            label = ",".join(f"{n}=None" for n in optional if n in nones)
            out.append(Variant(nones, f"{name}[{label}]" if nones else name, ov, scope))
        return out

    def module_class(self, ann: ast.expr) -> ModuleClass | None:
        """The module class an annotation names, e.g. nn.Dropout."""
        return self.class_named(ann)

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
        errors: list[FrontendError] = []
        cls.attrs = declared_attributes(node, cls.ints, stub=False, errors=errors)
        self.errors += [Error(e.line or node.lineno, node.name, e.message) for e in errors]
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

    def is_local(self, name: str) -> bool:
        """A local variable of the body translated: a value, a module
        parameter, or None."""
        return name in self.locals or name in self.module_locals or name in self.nones

    def class_named(self, e: ast.expr) -> ModuleClass | None:
        """The module class an expression like nn.Linear or Encoder names."""
        if isinstance(e, ast.Name) and not self.is_local(e.id) and e.id in self.classes:
            return self.classes[e.id]
        q = self.qualname(e)
        return None if q is None else self.stubs.classes.get(q)

    def instance(self, e: ast.expr) -> Instance | None:
        """The module e is, if it's self, a module parameter, or a module
        attribute of one of those."""
        if isinstance(e, ast.Name):
            if e.id in self.module_locals:
                return self.module_locals[e.id]
            if self.cls is None or e.id != self.self_name or self.is_local(e.id):
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
        saved = (
            self.locals,
            self.module_locals,
            self.nones,
            self.terms,
            self.cls,
            self.self_name,
            self.in_init,
            self.in_instance,
        )
        self.locals, self.module_locals, self.nones = {n: n for n in cls.ints}, {}, set()
        self.terms = {}
        self.cls, self.self_name, self.in_init, self.in_instance = cls, cls.self_name, False, True
        try:
            return f()
        finally:
            (
                self.locals,
                self.module_locals,
                self.nones,
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
        return self.built_instance(base, name, value, e)

    def list_instance(self, e: ast.expr) -> Instance | None:
        """The modules of an nn.ModuleList attribute such as self.layers. They
        are all built by the same call, so one instance stands for each."""
        if not isinstance(e, ast.Attribute):
            return None
        base = self.instance(e.value)
        if base is None or not base.cls.user or e.attr not in base.cls.fields:
            return None
        value = self.field(base.cls, e.attr, e)
        element = self.as_instance(base.cls, lambda: self.list_element(value))
        return None if element is None else self.built_instance(base, e.attr, element, e)

    def list_element(self, value: ast.expr) -> ast.Call | None:
        """For nn.ModuleList([C(...) for _ in range(n)]), the call that builds
        each module; None if value isn't an nn.ModuleList."""
        if not (isinstance(value, ast.Call) and self.qualname(value.func) == MODULE_LIST):
            return None
        comp = value.args[0] if len(value.args) == 1 and not value.keywords else None
        if not (isinstance(comp, ast.ListComp) and len(comp.generators) == 1):
            raise FrontendError(f"an nn.ModuleList must be built as {MODULE_LIST_FORM}", value)
        gen = comp.generators[0]
        if (
            gen.ifs
            or gen.is_async
            or not isinstance(gen.iter, ast.Call)
            or not isinstance(gen.iter.func, ast.Name)
            or gen.iter.func.id != "range"
            or self.is_local("range")
        ):
            raise FrontendError(f"an nn.ModuleList must be built as {MODULE_LIST_FORM}", gen)
        targets = {n.id for n in ast.walk(gen.target) if isinstance(n, ast.Name)}
        if any(isinstance(n, ast.Name) and n.id in targets for n in ast.walk(comp.elt)):
            raise FrontendError(
                "the modules of an nn.ModuleList must all be built alike, so the loop "
                "variable can't be used",
                comp.elt,
            )
        if not (isinstance(comp.elt, ast.Call) and self.class_named(comp.elt.func) is not None):
            raise FrontendError(f"an nn.ModuleList must be built as {MODULE_LIST_FORM}", comp.elt)
        return comp.elt

    def built_instance(self, base: Instance, name: str, value: ast.Call, e: ast.AST) -> Instance:
        """The module the call value builds for base.name in __init__."""
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
        if name in inst.cls.attrs:
            raise FrontendError(f"`{name}` isn't a module, so it can't be called", e)
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
            return self.user_call(key, f, inst.ints, args, keywords, e)
        callee = self.stubs.functions.get(key)
        if callee is None:
            raise FrontendError(f"no stub for `{key}`", e)
        self.use_init(inst.cls)
        return self.stub_call(callee, args, keywords, e, prefix=inst.ints)

    def use_init(self, cls: ModuleClass) -> None:
        """A stub class's instances satisfy its constructor's requires, so the
        checker needs its signature."""
        init = self.stubs.functions.get(cls.init)
        if not cls.user and init is not None:
            self.env[init.name] = [ov.sig for ov in init.overloads]

    def attribute_call(self, inst: Instance, attr: str, value: Json | None = None) -> Json:
        """Reading a declared attribute, or with a value, assigning it: calls
        to functions of the instance's dims, whose types are the declaration
        (see attribute_signatures)."""
        getter, setter = attribute_signatures(inst.cls.ints, inst.cls.attrs[attr])
        if inst.cls.ints:
            # the declaration may rely on the class invariant
            getter["instances"] = setter["instances"] = inst.cls.instances()
        self.use_init(inst.cls)
        if value is None:
            self.env[inst.cls.getter(attr)] = [getter]
            return ir.Call(inst.cls.getter(attr), list(inst.ints))
        self.env[inst.cls.setter(attr)] = [setter]
        return ir.Call(inst.cls.setter(attr), list(inst.ints) + [value])

    def user_call(
        self,
        key: str,
        f: Function,
        prefix: list[Json],
        args: list[Arg],
        keywords: dict[str, ast.expr],
        e: ast.AST,
    ) -> Json:
        """A call to a user function or method (after prefix, its instance
        dims). The Optional parameters given None select the variant called,
        and a module argument is passed as its dims."""
        if f.overload is None:
            raise FrontendError(f"`{key}`'s signature has errors", e)
        params = f.overload.params
        try:
            terms = self.bind(params, args, keywords)
        except BindError as err:
            raise FrontendError(f"`{key}` doesn't take these arguments: {err}", e) from None
        nones = frozenset(p.name for p, t in zip(params, terms) if t is NONE)
        out = list(prefix)
        for p, t in zip(params, terms):
            if t is not NONE:
                out += t if p.module is not None else [t]
        return ir.Call(f.variant(nones).name, out)

    # ---- statements ----

    def body(self, f: Function, v: Variant) -> list[Json]:
        params = v.overload.params
        present = [p for p in params if p.name not in v.nones]
        self.locals = {p.name: p.ir_name for p in present if p.module is None}
        self.module_locals = {
            p.name: Instance(p.module, [ir.Var(n) for n in p.ints])
            for p in present
            if p.module is not None
        }
        self.nones = set(v.nones)
        self.after_loop = set()
        self.sig = v.overload.sig
        self.terms = {}
        self.scope = v.scope
        self.cls = f.cls
        self.in_init = f.cls is not None and f.node.name == "__init__"
        self.self_name = f.node.args.args[0].arg if f.cls is not None else None
        self.returns_none = v.overload.sig["ret"] == ir.NoneType()
        stmts = list(f.node.body)
        if stmts and is_docstring(stmts[0]):
            stmts.pop(0)
        out, returned = self.block(stmts)
        # falling off the end returns None
        if self.returns_none and not returned:
            out.append(ir.Return(ir.Tuple([])))
        return out

    def block(self, stmts: list[ast.stmt]) -> tuple[list[Json], bool]:
        """The statements' IR, and whether they return. An `if` on whether a
        local is None is decided here, so only the branch taken is
        translated."""
        out: list[Json] = []
        for s in stmts:
            if isinstance(s, ast.Pass):
                continue
            if isinstance(s, ast.Assert):
                out += self.assertion(s)
                continue
            if self.in_init and is_super_init(s):
                continue  # nn.Module's __init__
            if isinstance(s, ast.For):
                out.append(ir.At(s.lineno, self.text(s), self.loop(s)))
                continue
            if isinstance(s, ast.If):
                taken = s.body if self.static_test(s.test, s, "`if`") else s.orelse
                inner, returned = self.block(taken)
                out += inner
                if returned:
                    return out, True
                continue
            if (
                isinstance(s, ast.Assign)
                and len(s.targets) == 1
                and isinstance(s.targets[0], ast.Name)
                and is_none(s.value)
            ):
                # x = None: known statically, like a None parameter
                name = s.targets[0].id
                self.assign(name)
                del self.locals[name]
                self.nones.add(name)
                continue
            out.append(ir.At(s.lineno, self.text(s), self.stmt(s)))
            if isinstance(s, ast.Return):
                return out, True
        return out, False

    def loop(self, s: ast.For) -> Json:
        """for layer in self.layers:, over an nn.ModuleList. The body is
        translated once, with layer as the list's instance, and each local it
        reassigns must keep its shape: that's the loop's invariant, so one
        check covers any number of iterations."""
        inst = self.list_instance(s.iter)
        if inst is None:
            raise FrontendError(
                "`for` is only supported over an nn.ModuleList attribute, e.g. "
                f"`for layer in self.layers:`: `{self.text(s)}`",
                s,
            )
        if s.orelse:
            raise FrontendError("`for ... else` isn't supported", s)
        if not isinstance(s.target, ast.Name):
            raise FrontendError("the loop variable must be a name", s.target)
        locals_before = dict(self.locals)
        modules_before = dict(self.module_locals)
        nones_before = set(self.nones)
        assigned = {
            n.id
            for stmt in s.body
            for n in ast.walk(stmt)
            if isinstance(n, ast.Name) and isinstance(n.ctx, ast.Store)
        }
        modules = sorted(assigned & modules_before.keys())
        if modules:
            raise FrontendError(f"the loop can't reassign the module `{modules[0]}`", s)
        target = s.target.id
        self.assign(target)
        del self.locals[target]
        self.module_locals[target] = inst
        body, returned = self.block(s.body)
        if returned:
            raise FrontendError("`return` inside a loop isn't supported", s)
        carried = sorted(assigned & locals_before.keys() - {target})
        for name in carried:
            if name not in self.locals:
                raise FrontendError(f"`{name}` must keep its shape in the loop", s)
        pairs = [[locals_before[name], self.locals[name]] for name in carried]
        # afterwards, a local the body reassigns has its shape from before the
        # loop. one the body binds, and the loop variable, may be unbound or
        # hold anything, so they're dropped
        self.locals, self.module_locals, self.nones = locals_before, modules_before, nones_before
        for name in (assigned | {target}) - set(carried):
            self.locals.pop(name, None)
            self.module_locals.pop(name, None)
            self.nones.discard(name)
            self.after_loop.add(name)
        return ir.Loop(pairs, body)

    def static_test(self, test: ast.expr, node: ast.AST, what: str) -> bool:
        """The value of a test on whether locals are None, which is known in
        each variant: x is None, x is not None, and not/and/or of those."""
        if isinstance(test, ast.UnaryOp) and isinstance(test.op, ast.Not):
            return not self.static_test(test.operand, node, what)
        if isinstance(test, ast.BoolOp):
            # short-circuiting, so a later operand may be one that isn't known
            stop = isinstance(test.op, ast.Or)
            for v in test.values:
                if self.static_test(v, node, what) == stop:
                    return stop
            return not stop
        if (
            isinstance(test, ast.Compare)
            and len(test.ops) == 1
            and isinstance(test.ops[0], (ast.Is, ast.IsNot))
        ):
            left, right = test.left, test.comparators[0]
            if is_none(left):
                left, right = right, left
            if is_none(right) and isinstance(left, ast.Name):
                if not self.is_local(left.id):
                    raise FrontendError(f"`{left.id}` isn't a local variable", left)
                none = left.id in self.nones
                return none if isinstance(test.ops[0], ast.Is) else not none
        raise FrontendError(
            f"{what} isn't supported yet, except on whether variables are None "
            f"(`x is None`, `x is not None`): `{self.text(node)}`",
            node,
        )

    # ---- asserts ----

    def assertion(self, s: ast.Assert) -> list[Json]:
        """An assert, which the rest of the body may assume: comparisons of
        ints with + - * //, and `a % b == 0`, written b * (a // b) == a. A call
        or attribute in one, like x.size(1), is evaluated first. Other
        conjuncts are dropped, which is sound.

        In __init__, a fact about the instance dims holds for every instance,
        so it's also an ensures of the constructor, part of the class
        invariant."""
        lets: list[tuple[str, Json]] = []
        cs: list[Json] = []
        conjuncts = (
            s.test.values
            if isinstance(s.test, ast.BoolOp) and isinstance(s.test.op, ast.And)
            else [s.test]
        )
        for test in conjuncts:
            mark = len(lets)
            try:
                cs += self.comparison(test, lets)
            except (Unstated, FrontendError, BindError):
                del lets[mark:]
        text = self.text(s)
        out = [ir.At(s.lineno, text, ir.Let(x, t)) for x, t in lets]
        out += [ir.At(s.lineno, text, ir.Assume(c)) for c in cs]
        if self.in_init:
            assert self.cls is not None
            self.sig["ensures"] += [c for c in cs if about_ints(c, self.cls.ints)]
        return out

    def comparison(self, test: ast.expr, lets: list[tuple[str, Json]]) -> list[Json]:
        if not isinstance(test, ast.Compare):
            raise Unstated
        a, b = test.left, test.comparators[0]
        if len(test.ops) == 1 and isinstance(test.ops[0], ast.Eq):
            # a % b == 0: b divides a
            mod = a if is_zero(b) else b if is_zero(a) else None
            if isinstance(mod, ast.BinOp) and isinstance(mod.op, ast.Mod):
                x = self.assert_arith(mod.left, lets)
                y = self.assert_arith(mod.right, lets)
                return [["Eq", ir.binop("Mul", y, ir.binop("Div", x, y)), x]]
        out = []
        left = self.assert_arith(a, lets)
        for op, right_e in zip(test.ops, test.comparators):
            right = self.assert_arith(right_e, lets)
            if isinstance(op, ast.Eq):
                out.append(["Eq", left, right])
            elif isinstance(op, ast.LtE):
                out.append(["Le", left, right])
            elif isinstance(op, ast.Lt):
                out.append(["Lt", left, right])
            elif isinstance(op, ast.GtE):
                out.append(["Le", right, left])
            elif isinstance(op, ast.Gt):
                out.append(["Lt", right, left])
            else:
                raise Unstated
            left = right
        return out

    def assert_arith(self, e: ast.expr, lets: list[tuple[str, Json]]) -> Json:
        """An int expression in an assert, over the body's int locals."""
        ops = {ast.Add: "Add", ast.Sub: "Sub", ast.Mult: "Mul", ast.FloorDiv: "Div"}
        if isinstance(e, ast.BinOp) and type(e.op) in ops:
            return ir.binop(
                ops[type(e.op)], self.assert_arith(e.left, lets), self.assert_arith(e.right, lets)
            )
        if isinstance(e, ast.Constant) and isinstance(e.value, (bool, int)):
            return ir.Int(int(e.value))
        if isinstance(e, ast.UnaryOp) and isinstance(e.op, ast.USub):
            return ir.binop("Sub", ir.Int(0), self.assert_arith(e.operand, lets))
        if isinstance(e, ast.Name) and e.id in self.locals:
            return ir.Id(self.locals[e.id])
        if isinstance(e, (ast.Call, ast.Attribute)):
            # evaluated first, into a local that no Python name can clash with
            name = f"({ast.unparse(e)})"
            lets.append((name, self.term(e)))
            return ir.Id(name)
        raise Unstated

    def stmt(self, s: ast.stmt) -> Json:
        if isinstance(s, ast.Assign):
            target = s.targets[0] if len(s.targets) == 1 else None
            if isinstance(target, ast.Tuple) and all(isinstance(x, ast.Name) for x in target.elts):
                t = self.term(s.value)
                return ir.Unpack([self.assign(x.id) for x in target.elts], t)
            if isinstance(target, ast.Attribute) and self.instance(target.value) is not None:
                return self.set_attribute(target, s.value)
            if isinstance(target, ast.Subscript):
                return self.set_slice(target, s.value, s)
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
        if isinstance(s, ast.Expr) and self.cls is not None and self.self_name is not None:
            buffer = buffer_of(s.value, self.self_name)
            if buffer is not None and not self.is_local(self.self_name):
                # like self.name = value
                name, value = buffer
                target = ast.Attribute(ast.Name(self.self_name, ast.Load()), name, ast.Store())
                return self.set_attribute(ast.copy_location(target, s), value)
        if isinstance(s, ast.Return):
            if s.value is None or (isinstance(s.value, ast.Constant) and s.value.value is None):
                if not self.returns_none:
                    raise FrontendError("a checked function must return a value", s)
                return ir.Return(ir.Tuple([]))
            return ir.Return(self.term(s.value))
        kind = {
            ast.If: "`if`", ast.For: "`for`", ast.While: "`while`", ast.With: "`with`",
            ast.Try: "`try`", ast.FunctionDef: "a nested function",
            ast.Expr: "an expression statement", ast.Break: "`break`",
            ast.Continue: "`continue`",
        }.get(type(s), "this statement")  # fmt: skip
        raise FrontendError(f"{kind} isn't supported yet: `{self.text(s)}`", s)

    def assign(self, name: str) -> str:
        self.nones.discard(name)
        self.after_loop.discard(name)
        self.module_locals.pop(name, None)
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
        inst = self.instance(target.value)
        assert inst is not None
        if target.attr in inst.cls.attrs:
            # checked against the declaration, which is what reads of it give
            t = self.attribute_call(inst, target.attr, self.term(value))
            return ir.Let(ast.unparse(target), t)
        if not isinstance(target.value, ast.Name):
            raise FrontendError(
                f"assigning to an attribute of a module (`{ast.unparse(target)}`) is only "
                f"supported when `{inst.cls.name}` declares it, e.g. "
                f'`{target.attr}: Float[Tensor, "..."]`',
                target,
            )
        element = self.list_element(value)
        if element is not None:
            # the modules are all built alike, so checking one constructor
            # call checks them all (and asks for more when there are none)
            value = element
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
            if e.id in self.nones:
                raise FrontendError(f"`{e.id}` is None here", e)
            if self.instance(e) is not None:
                raise FrontendError(
                    f"`{e.id}` can only be used for its attributes and methods here", e
                )
            if self.in_instance:
                raise FrontendError(f"`{e.id}` isn't an int parameter of `__init__`", e)
            if e.id in self.after_loop:
                raise FrontendError(
                    f"`{e.id}` is assigned in a loop's body, which may not run, so it isn't "
                    "known after the loop",
                    e,
                )
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
        if isinstance(e, ast.IfExp):
            test = self.static_test(e.test, e, "a conditional expression")
            return self.term(e.body if test else e.orelse)
        if isinstance(e, ast.Tuple):
            if any(isinstance(x, ast.Starred) for x in e.elts):
                raise FrontendError("starred items in tuples aren't supported", e)
            return ir.Tuple([self.term(x) for x in e.elts])
        if isinstance(e, ast.Subscript):
            if isinstance(e.value, ast.Attribute) and e.value.attr == "shape":
                raise FrontendError("`x.shape[i]` isn't supported yet", e)
            return self.subscript(e)
        raise FrontendError(f"unsupported expression `{ast.unparse(e)}`", e)

    def subscript(self, e: ast.Subscript) -> Json:
        """x[a:b:c, ...]: slices of x's leading dims, with Python's rules for
        negative and out-of-range bounds (see slice_dim in the checker)."""
        items = e.slice.elts if isinstance(e.slice, ast.Tuple) else [e.slice]
        out = []
        for item in items:
            if not isinstance(item, ast.Slice):
                raise FrontendError(
                    "only slices like `x[a:b]` or `x[:, ::2]` are supported in indexing yet: "
                    f"`{self.text(e)}`",
                    e,
                )
            parts = (item.lower, item.upper, item.step)
            out.append([None if p is None or is_none(p) else self.term(p) for p in parts])
        return ir.Slice(self.term(e.value), out)

    def set_slice(self, target: ast.Subscript, value: ast.expr, s: ast.stmt) -> Json:
        """x[a:b] = value, which assigns into x in place: the value must
        broadcast to the slice, and x keeps its shape."""
        if not (isinstance(target.value, ast.Name) and target.value.id in self.locals):
            raise FrontendError("only slices of local variables can be assigned", target)
        callee = self.stubs.functions.get("operator.setitem")
        if callee is None:
            raise FrontendError("no stub for slice assignment (operator.setitem)", s)
        t = self.stub_call(callee, [("target", self.subscript(target)), value], {}, s)
        return ir.Let(f"({self.text(target)})", t)

    def qualname(self, e: ast.expr) -> str | None:
        """The qualified name an expression like F.relu or torch.nn.functional
        refers to, if it's rooted at an import."""
        if isinstance(e, ast.Name):
            if self.is_local(e.id):
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
        if isinstance(f, ast.Name) and not self.is_local(f.id) and f.id in self.functions:
            return self.user_call(f.id, self.functions[f.id], [], args, keywords, e)
        q = self.qualname(f)
        if q is not None:
            callee = self.stubs.functions.get(q)
            if callee is None:
                raise FrontendError(f"no stub for `{q}`", e)
            return self.stub_call(callee, args, keywords, e)
        if isinstance(f, ast.Name):
            if f.id in self.after_loop:
                self.term(f)  # the error for a name bound only in a loop
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
            if e.attr in base.cls.attrs:
                return self.attribute_call(base, e.attr)
            if not base.cls.user:
                raise FrontendError(f"no stub declares the attribute `{base.cls.name}.{e.attr}`", e)
            key = f"{base.cls.name}.{e.attr}"
            if e.attr not in base.cls.fields and (key in self.functions or key in self.unannotated):
                raise FrontendError(f"`{ast.unparse(e)}` is a method; call it", e)
            if self.instance(e) is not None:
                raise FrontendError(
                    f"`{ast.unparse(e)}` is a module; only calls to it are supported", e
                )
            if self.list_instance(e) is not None:
                raise FrontendError(
                    f"`{ast.unparse(e)}` is an nn.ModuleList; only loops over it are "
                    f"supported (`for layer in {ast.unparse(e)}:`)",
                    e,
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

    def none_value(self, a: Arg) -> bool:
        """Whether an argument is None: the literal, or a local that's None."""
        if isinstance(a, tuple):
            return False
        return is_none(a) or (isinstance(a, ast.Name) and a.id in self.nones)

    def arg_term(self, a: Arg, p: Param | None) -> Json:
        """The term for an argument. For an Optional parameter it may be NONE,
        and for a module parameter it's the instance's dims."""
        if isinstance(a, tuple):
            return a[1]
        if p is not None and self.none_value(a):
            if p.optional:
                return NONE
            if isinstance(a, ast.Name):
                raise FrontendError(f"`{a.id}` is None here", a)
            raise BindError(f"`{p.name}` can't be None")
        if p is not None and p.module is not None:
            inst = self.instance(a)
            if inst is None or inst.cls is not p.module:
                raise BindError(f"`{p.name}` takes a `{p.module.name}` module")
            return inst.ints
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
        name = None
        if isinstance(s, ast.Assign) and len(s.targets) == 1:
            target, value = s.targets[0], s.value
        elif isinstance(s, ast.AnnAssign):
            target, value = s.target, s.value
        elif isinstance(s, ast.Expr) and buffer_of(s.value, self_name) is not None:
            target = s.value
            name, value = buffer_of(s.value, self_name)
        name = name or (None if target is None else attribute(target))
        if name is not None and value is not None:
            top.add(id(target))
            fields[name] = value if name not in fields else None
    for node in ast.walk(init):
        name = None
        if isinstance(node, ast.Attribute) and isinstance(node.ctx, (ast.Store, ast.Del)):
            name = attribute(node)
        elif isinstance(node, ast.Call) and buffer_of(node, self_name) is not None:
            name = buffer_of(node, self_name)[0]
        if name is not None and id(node) not in top:
            fields[name] = None
    return fields


def buffer_of(e: ast.expr, self_name: str) -> tuple[str, ast.expr] | None:
    """self.register_buffer("name", value), which is like self.name = value:
    the name and the value."""
    if (
        isinstance(e, ast.Call)
        and isinstance(e.func, ast.Attribute)
        and e.func.attr == "register_buffer"
        and isinstance(e.func.value, ast.Name)
        and e.func.value.id == self_name
        and len(e.args) == 2
        and isinstance(e.args[0], ast.Constant)
        and isinstance(e.args[0].value, str)
        and all(k.arg == "persistent" for k in e.keywords)
    ):
        return e.args[0].value, e.args[1]
    return None


def is_zero(e: ast.expr) -> bool:
    return isinstance(e, ast.Constant) and type(e.value) is int and e.value == 0


def about_ints(c: Json, ints: list[str]) -> bool:
    """Whether a constraint is only about the instance dims ints, with
    divisors that are instance dims (which the constructor infers positive)
    or positive numbers: so it can be stated wherever the invariant is."""

    def ok(e: Json) -> bool:
        if e[0] == "Id":
            return e[1] in ints
        if e[0] == "Int":
            return True
        if e[0] == "Div":
            divisor = e[2]
            positive = divisor[0] == "Int" and divisor[1] > 0
            return ok(e[1]) and (positive or (divisor[0] == "Id" and divisor[1] in ints))
        return e[0] in ("Add", "Sub", "Mul") and ok(e[1]) and ok(e[2])

    return ok(c[1]) and ok(c[2])


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
