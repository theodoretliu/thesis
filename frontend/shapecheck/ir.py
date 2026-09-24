"""Constructors for the checker IR, encoded as JSON (see README.md).

Every IR value is a list tagged with the OCaml constructor's name, e.g.
["Id", "n"] or ["Call", "torch.matmul", [["Var", "x"], ["Var", "w"]]].
"""

from __future__ import annotations

import json
from typing import Any, Union

Json = Any
Idx = Union[str, int]  # a list index: an int parameter's name or a literal


# ---- dimension entries ----


def Id(x: str) -> Json:
    return ["Id", x]


def Int(i: int) -> Json:
    return ["Int", i]


def binop(op: str, a: Json, b: Json) -> Json:
    assert op in ("Add", "Sub", "Mul", "Div")
    return [op, a, b]


def Spread(a: str) -> Json:
    return ["Spread", a]


def Broadcast(a: str) -> Json:
    return ["Broadcast", a]


def Broadcasted(names: list[str]) -> Json:
    return ["Broadcasted", names]


def Prod(a: str) -> Json:
    return ["Prod", a]


def Rank(a: str) -> Json:
    return ["Rank", a]


def Index(a: str, i: Idx) -> Json:
    return ["Index", a, i]


# ---- types and constraints ----


def Array(entries: list[Json]) -> Json:
    return ["Array", entries]


def IntType() -> Json:
    return ["Int"]


def IntExpr(e: Json) -> Json:
    return ["IntExpr", e]


def Literal(i: int) -> Json:
    return ["Literal", i]


def TupleType(typs: list[Json]) -> Json:
    return ["Tuple", typs]


def signature(
    params: list[Json],
    ret: Json,
    requires: list[Json],
    exists: list[str],
    ensures: list[Json],
) -> Json:
    return {
        "params": params,
        "ret": ret,
        "requires": requires,
        "exists": exists,
        "ensures": ensures,
    }


# ---- bodies ----


def Var(x: str) -> Json:
    return ["Var", x]


def Lit(i: int) -> Json:
    return ["Lit", i]


def Call(f: str, args: list[Json]) -> Json:
    return ["Call", f, args]


def Shape(ints: list[Json]) -> Json:
    return ["Shape", ints]


def Scalar() -> Json:
    return ["Scalar"]


def Tuple(terms: list[Json]) -> Json:
    return ["Tuple", terms]


def Let(x: str, t: Json) -> Json:
    return ["Let", x, t]


def LetAnnot(x: str, typ: Json, t: Json) -> Json:
    return ["LetAnnot", x, typ, t]


def Return(t: Json) -> Json:
    return ["Return", t]


def Unpack(names: list[str], t: Json) -> Json:
    return ["Unpack", names, t]


def At(line: int, text: str, stmt: Json) -> Json:
    return ["At", line, text, stmt]


# ---- printing ----


def dumps(value: Json, width: int = 88) -> str:
    """JSON with small values kept on one line, for --dump-ir and tests."""

    def go(v: Json, indent: str) -> str:
        flat = json.dumps(v)
        if len(indent) + len(flat) <= width or not isinstance(v, (list, dict)):
            return flat
        inner = indent + "  "
        if isinstance(v, dict):
            items = [f"{inner}{json.dumps(k)}: {go(x, inner)}" for k, x in v.items()]
            return "{\n" + ",\n".join(items) + "\n" + indent + "}"
        items = [inner + go(x, inner) for x in v]
        return "[\n" + ",\n".join(items) + "\n" + indent + "]"

    return go(value, "")
