"""Parsing jaxtyping shape strings, e.g. "*batch n d", into dimension entries.

The mapping onto the checker's entries:

    n, seq      a named dim                   Id
    3           a fixed size                  Int
    *batch      a named run of dims           Spread
    ...  *_     an unnamed run of dims        a fresh Spread
    _  _foo     one dim that isn't checked    a fresh Id
    *#batch     batch again, broadcastable    Broadcast (a first occurrence binds it)
    #n          n or 1                        BroadcastDim (parameters only)
    dim-1       arithmetic on names and ints  Add/Sub/Mul/Div (+ - * //)

Stubs may also use list functions that jaxtyping doesn't have, since library
signatures need them: *drop(A,i), *keep(A,i), *permute(A,i,j), *swap(A,i,j),
*setat(A,i,d), *insertat(A,i,d), *broadcast(A,B), and prod(A), rank(A),
A[i] inside arithmetic.
"""

from __future__ import annotations

import ast
import re

from . import ir
from .ir import Json


class ShapeError(Exception):
    pass


class Scope:
    """The names bound so far by a signature's parameters (and, in a body,
    its annotations). Dims bind wherever they first appear; spreads must be
    bound before a return type may use them."""

    def __init__(self, stub: bool, int_params: set[str] = frozenset()):
        self.stub = stub
        self.int_params = set(int_params)
        self.dims: set[str] = set()
        self.spreads: set[str] = set()
        self.fresh = 0

    def fresh_name(self) -> str:
        self.fresh += 1
        return f"_{self.fresh}"


IDENT = re.compile(r"[A-Za-z_][A-Za-z0-9_]*\Z")
LIST_FUNCTIONS = ("drop", "keep", "permute", "swap", "setat", "insertat", "broadcast")


def tokens(s: str) -> list[str]:
    """Split on whitespace outside parentheses, so stubs may write
    *drop(A, dim)."""
    out, cur, depth = [], "", 0
    for c in s:
        if c in "([":
            depth += 1
        elif c in ")]":
            depth -= 1
        if c.isspace() and depth == 0:
            if cur:
                out.append(cur)
            cur = ""
        else:
            cur += c
    if depth != 0:
        raise ShapeError(f"unbalanced parentheses in {s!r}")
    if cur:
        out.append(cur)
    return out


def parse_shape(s: str, scope: Scope, binding: bool) -> list[Json]:
    """The entries of a shape string. binding is False for a return type,
    where new spreads can't appear (only new dims, which are existential)."""
    return [parse_token(t, scope, binding) for t in tokens(s)]


def parse_token(tok: str, scope: Scope, binding: bool) -> Json:
    if tok == "...":
        tok = "*_"
    if re.match(r"\*[a-z]+\(", tok):
        if not scope.stub:
            raise ShapeError(f"list functions like {tok!r} are only available in stubs")
        return list_function(tok[1:], scope)
    mods = ""
    while tok and tok[0] in "*#?":
        mods += tok[0]
        tok = tok[1:]
    if "?" in mods:
        raise ShapeError(f"`?` dims aren't supported (in {mods + tok!r})")
    variadic, broadcast = "*" in mods, "#" in mods
    anonymous = tok.startswith("_") and IDENT.match(tok)

    if variadic:
        if not IDENT.match(tok):
            raise ShapeError(f"expected a name after `*`, got {mods + tok!r}")
        if anonymous:
            if not binding:
                raise ShapeError(
                    "an unnamed run of dims (`...` or `*_`) in a return type: name "
                    "it, e.g. `*batch`, where the parameters bind it"
                )
            name = scope.fresh_name()
            scope.spreads.add(name)
            return ir.Spread(name)
        if tok in scope.dims:
            raise ShapeError(f"`{tok}` is used both as a dim and as `*{tok}`")
        if tok in scope.spreads:
            return ir.Broadcast(tok) if broadcast else ir.Spread(tok)
        if not binding:
            raise ShapeError(f"`*{tok}` in the return type isn't bound by a parameter")
        # a first occurrence binds tok exactly, even if it's marked `#`
        scope.spreads.add(tok)
        return ir.Spread(tok)

    if anonymous:
        return ir.Id(scope.fresh_name())
    if broadcast:
        if not IDENT.match(tok):
            raise ShapeError(f"expected a name after `#`, got {mods + tok!r}")
        if tok in scope.spreads:
            raise ShapeError(f"`{tok}` is used both as a dim and as `*{tok}`")
        if not binding:
            raise ShapeError(f"a broadcastable dim `#{tok}` can only be in a parameter's shape")
        # like a dim, it binds tok where it first appears
        scope.dims.add(tok)
        return ir.BroadcastDim(tok)
    if re.fullmatch(r"[0-9]+", tok):
        return ir.Int(int(tok))
    if IDENT.match(tok):
        if tok in scope.spreads:
            raise ShapeError(f"`{tok}` is used both as a dim and as `*{tok}`")
        if binding:
            scope.dims.add(tok)
        return ir.Id(tok)
    try:
        node = ast.parse(tok, mode="eval").body
    except SyntaxError:
        raise ShapeError(f"can't parse the dim {tok!r}") from None
    return arith(node, scope)


def arith(node: ast.expr, scope: Scope) -> Json:
    """An arithmetic dimension: + - * // over names and ints (and prod(A),
    rank(A), A[i] in stubs)."""
    ops = {ast.Add: "Add", ast.Sub: "Sub", ast.Mult: "Mul", ast.FloorDiv: "Div"}
    if isinstance(node, ast.BinOp) and type(node.op) in ops:
        return ir.binop(ops[type(node.op)], arith(node.left, scope), arith(node.right, scope))
    if isinstance(node, ast.BinOp) and isinstance(node.op, ast.Div):
        raise ShapeError("use // for division in a dim")
    if is_int(node):
        return ir.Int(node.value)
    if isinstance(node, ast.UnaryOp) and isinstance(node.op, ast.USub):
        if is_int(node.operand):
            return ir.Int(-node.operand.value)
        return ir.binop("Sub", ir.Int(0), arith(node.operand, scope))
    if isinstance(node, ast.Name):
        return ir.Id(node.id)
    if (
        scope.stub
        and isinstance(node, ast.Call)
        and isinstance(node.func, ast.Name)
        and node.func.id in ("prod", "rank")
        and len(node.args) == 1
        and isinstance(node.args[0], ast.Name)
        and not node.keywords
    ):
        make = ir.Prod if node.func.id == "prod" else ir.Rank
        return make(node.args[0].id)
    if scope.stub and isinstance(node, ast.Subscript) and isinstance(node.value, ast.Name):
        return ir.Index(node.value.id, index(node.slice))
    raise ShapeError(f"unsupported dim expression `{ast.unparse(node)}`")


def is_int(node: ast.expr) -> bool:
    return (
        isinstance(node, ast.Constant)
        and isinstance(node.value, int)
        and not isinstance(node.value, bool)
    )


def index(node: ast.expr) -> ir.Idx:
    """A list index: an int parameter's name or a literal."""
    if isinstance(node, ast.Name):
        return node.id
    if is_int(node):
        return node.value
    if isinstance(node, ast.UnaryOp) and isinstance(node.op, ast.USub) and is_int(node.operand):
        return -node.operand.value
    raise ShapeError(f"expected an index (a name or an int), got `{ast.unparse(node)}`")


def list_function(src: str, scope: Scope) -> Json:
    try:
        node = ast.parse(src, mode="eval").body
    except SyntaxError:
        raise ShapeError(f"can't parse *{src}") from None
    if not (
        isinstance(node, ast.Call)
        and isinstance(node.func, ast.Name)
        and node.func.id in LIST_FUNCTIONS
        and not node.keywords
        and node.args
        and all(isinstance(a, ast.expr) for a in node.args)
    ):
        raise ShapeError(
            f"unknown list function *{src}; expected one of " + ", ".join(LIST_FUNCTIONS)
        )
    f, args = node.func.id, node.args
    names = [a.id if isinstance(a, ast.Name) else None for a in args]
    if f == "broadcast":
        if None in names:
            raise ShapeError(f"*broadcast takes spread names, got *{src}")
        return ir.Broadcasted(names)
    if names[0] is None:
        raise ShapeError(f"*{f} needs a spread name first, got *{src}")
    a = names[0]
    if f in ("drop", "keep", "permute"):
        return [f.capitalize(), a, [index(i) for i in args[1:]]]
    if f == "swap":
        if len(args) != 3:
            raise ShapeError(f"*swap takes a spread and two indices, got *{src}")
        return ["Swap", a, index(args[1]), index(args[2])]
    if len(args) != 3:
        raise ShapeError(f"*{f} takes a spread, an index and a dim, got *{src}")
    if f == "setat":
        return ["SetAt", a, [index(args[1])], arith(args[2], scope)]
    return ["InsertAt", a, index(args[1]), arith(args[2], scope)]
