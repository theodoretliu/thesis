import ast
import textwrap
import unittest

from shapecheck.cli import DEFAULT_STUBS
from shapecheck.signatures import FrontendError
from shapecheck.translate import Stubs, load_stub_module, load_stubs, translate

STUBS = load_stubs([DEFAULT_STUBS])


def program(src, stubs=STUBS):
    prog, t = translate(textwrap.dedent(src), stubs)
    return prog, [e.message for e in t.errors]


def only_function(src):
    prog, errors = program(src)
    assert not errors, errors
    [f] = prog["functions"]
    return f


def stub_sig(src):
    stubs = Stubs()
    load_stub_module(stubs, "m", ast.parse(textwrap.dedent(src)))
    [callee] = stubs.functions.values()
    return [ov.sig for ov in callee.overloads]


def body(src):
    """The body's statements without their At wrappers."""
    return [s[3] for s in only_function(src)["body"]]


class Signatures(unittest.TestCase):
    def test_jaxtyping_signature(self):
        f = only_function(
            """
            def f(
                x: Float[Tensor, "*b n"], k: int, p: float, flag: Literal[True]
            ) -> Float[Tensor, "*b m"]:
                return x
            """
        )
        self.assertEqual(
            f["sig"],
            {
                "params": [
                    ["x", ["Array", [["Spread", "b"], ["Id", "n"]]]],
                    ["k", ["Int"]],
                    ["p", ["Array", []]],
                    ["flag", ["Literal", 1]],
                ],
                "ret": ["Array", [["Spread", "b"], ["Id", "m"]]],
                "requires": [],
                "exists": ["m"],  # only the return type names m
                "ensures": [],
            },
        )

    def test_parameter_names_apart_from_dims(self):
        f = only_function(
            """
            def f(n: Float[Tensor, "n"], d: Float[Tensor, "d"]) -> Float[Tensor, "d"]:
                return n
            """
        )
        self.assertEqual([p[0] for p in f["sig"]["params"]], ["n'", "d'"])
        self.assertEqual(f["body"][0][3], ["Return", ["Var", "n'"]])

    def test_int_parameters_are_their_dims(self):
        # a dim named after an int parameter is its value, not existential
        f = only_function(
            """
            def f(x: Float[Tensor, "n"], n: int, d: int) -> Float[Tensor, "n d"]:
                return x
            """
        )
        self.assertEqual([p[0] for p in f["sig"]["params"]], ["x", "n", "d"])
        self.assertEqual(f["sig"]["exists"], [])

    def test_tuple_return_type(self):
        f = only_function(
            """
            def f(x: Float[Tensor, "n"]) -> tuple[Float[Tensor, "n"], Float[Tensor, "m"]]:
                return x, x
            """
        )
        self.assertEqual(
            f["sig"]["ret"],
            ["Tuple", [["Array", [["Id", "n"]]], ["Array", [["Id", "m"]]]]],
        )
        self.assertEqual(f["sig"]["exists"], ["m"])
        self.assertEqual(f["body"][0][3], ["Return", ["Tuple", [["Var", "x"], ["Var", "x"]]]])

    def test_forward_reference_and_unshaped_array(self):
        f = only_function(
            """
            def f(x: "Float[Tensor, 'n']", y: torch.Tensor) -> Float[Tensor, "n"]:
                return x
            """
        )
        self.assertEqual(f["sig"]["params"][1], ["y", ["Array", [["Spread", "_1"]]]])

    def test_signature_errors(self):
        for src, msg in [
            ("def f(x) -> int: return x", "parameter x needs an annotation"),
            ("def f(x: int, **kw) -> int: return x", r"\*\*kwargs"),
            ("def f(*xs: int) -> int: return 0", r"\*args is only supported in stubs"),
            ("def f(x: str) -> int: return 0", "unsupported annotation `str`"),
            ("def f(x: int) -> Tensor: return 0", "needs a shape"),
            (
                'def f(x: Float[Tensor, "n"]) -> Float[Tensor, "#n"]: return x',
                "only be in a parameter's shape",
            ),
            ("def f(x: tuple[int, int]) -> int: return 0", "only supported as return types"),
            ("def f(x: int) -> tuple[int, ...]: return 0", "only fixed-length tuples"),
        ]:
            with self.subTest(src):
                _, errors = program(src)
                self.assertRegex(" ".join(errors), msg)

    def test_stub_asserts(self):
        [sig] = stub_sig(
            """
            def unique(x: Shaped[Tensor, "n"], k: int) -> Shaped[Tensor, "m"]:
                assert k <= n
                assert m <= n and m >= 1
            """
        )
        self.assertEqual(sig["exists"], ["m"])
        self.assertEqual(sig["requires"], [["Le", ["Id", "k"], ["Id", "n"]]])
        self.assertEqual(
            sig["ensures"],
            [["Le", ["Id", "m"], ["Id", "n"]], ["Le", ["Int", 1], ["Id", "m"]]],
        )

    def test_stub_conventions(self):
        [sig] = stub_sig(
            """
            def f(x: Shaped[Tensor, "*A"], dim: int, *shape: Shape["*B"]) -> Dim["rank(A)+dim"]: ...
            """
        )
        self.assertEqual(sig["params"][2], ["shape", ["Array", [["Spread", "B"]]]])
        self.assertEqual(sig["ret"], ["IntExpr", ["Add", ["Rank", "A"], ["Id", "dim"]]])
        [size] = stub_sig('def size(x: Shaped[Tensor, "*A"], dim: int) -> Dim["A[dim]"]: ...')
        self.assertEqual(size["ret"], ["IntExpr", ["Index", "A", "dim"]])

    def test_stub_body_must_be_asserts(self):
        with self.assertRaisesRegex(FrontendError, "only contain asserts"):
            stub_sig("def f(x: int) -> int:\n    return x\n")

    def test_stub_modules(self):
        self.assertIn("torch.nn.functional", STUBS.modules)
        self.assertIn("torch.nn.functional.linear", STUBS.functions)
        self.assertEqual(STUBS.methods["sum"], ["torch.Tensor.sum"])
        self.assertEqual(STUBS.properties["mT"], ["torch.Tensor.mT"])


class Bodies(unittest.TestCase):
    def test_statements(self):
        f = only_function(
            """
            def f(x: Float[Tensor, "n"]) -> Float[Tensor, "n"]:
                '''docstrings are skipped, and asserts are assumed'''
                assert x.ndim == 1
                y = x
                z: Float[Tensor, "n"] = y
                return z
            """
        )
        assertion = "assert x.ndim == 1"
        self.assertEqual(
            f["body"],
            [
                # x.ndim is evaluated into a local no Python name can clash with
                [
                    "At",
                    4,
                    assertion,
                    ["Let", "(x.ndim)", ["Call", "torch.Tensor.ndim", [["Var", "x"]]]],
                ],
                ["At", 4, assertion, ["Assume", ["Eq", ["Id", "(x.ndim)"], ["Int", 1]]]],
                ["At", 5, "y = x", ["Let", "y", ["Var", "x"]]],
                [
                    "At",
                    6,
                    'z: Float[Tensor, "n"] = y',
                    ["LetAnnot", "z", ["Array", [["Id", "n"]]], ["Var", "y"]],
                ],
                ["At", 7, "return z", ["Return", ["Var", "z"]]],
            ],
        )

    def test_operators_and_literals(self):
        [ret] = body(
            """
            def f(x: Float[Tensor, "n"]) -> Float[Tensor, "n"]:
                return -x @ x * 2.5 + (-1)
            """
        )
        self.assertEqual(
            ret,
            [
                "Return",
                [
                    "Call",
                    "operator.add",
                    [
                        [
                            "Call",
                            "operator.mul",
                            [
                                [
                                    "Call",
                                    "operator.matmul",
                                    [["Call", "operator.neg", [["Var", "x"]]], ["Var", "x"]],
                                ],
                                ["Scalar"],
                            ],
                        ],
                        ["Lit", -1],
                    ],
                ],
            ],
        )

    def test_names_resolve_through_imports(self):
        [a, b, c] = body(
            """
            import torch
            import torch.nn.functional as F
            from torch.nn.functional import relu as r

            def f(x: Float[Tensor, "n"]) -> Float[Tensor, "n"]:
                a = torch.relu(x)
                b = F.relu(a)
                return r(b)
            """
        )
        self.assertEqual(a[2][1], "torch.relu")
        self.assertEqual(b[2][1], "torch.nn.functional.relu")
        self.assertEqual(c[1][1], "torch.nn.functional.relu")

    def test_methods_properties_and_keywords(self):
        [s] = body(
            """
            def f(x: Float[Tensor, "a b"]) -> Float[Tensor, "a 1"]:
                return x.T.sum(keepdim=True, dim=0)
            """
        )
        # overloads 2 and 3 take these keywords (the checker then picks 3 by
        # keepdim's Literal type); keepdim=True is Lit 1
        self.assertEqual(
            s,
            [
                "Return",
                [
                    "Call",
                    "torch.Tensor.sum (overloads 2, 3)",
                    [["Call", "torch.Tensor.T", [["Var", "x"]]], ["Lit", 0], ["Lit", 1]],
                ],
            ],
        )

    def test_defaults_are_filled_in(self):
        prog, errors = program(
            """
            def f(x: Float[Tensor, "a b"]) -> Float[Tensor, "a"]:
                return x.sum(1)
            """
        )
        self.assertEqual(errors, [])
        self.assertEqual(
            prog["functions"][0]["body"][0][3][1],
            ["Call", "torch.Tensor.sum (overload 2)", [["Var", "x"], ["Lit", 1], ["Lit", 0]]],
        )
        self.assertEqual([e["name"] for e in prog["env"]], ["torch.Tensor.sum (overload 2)"])

    def test_shapes_from_ints(self):
        a, b, c = body(
            """
            import torch
            def f(x: Float[Tensor, "a b"], n: int) -> Float[Tensor, "n"]:
                y = torch.zeros(n, 2)
                z = x.reshape((2, n))
                return torch.reshape(x, (n,))
            """
        )
        self.assertEqual(a[2][2], [["Shape", [["Var", "n"], ["Lit", 2]]]])
        self.assertEqual(b[2][2][1], ["Shape", [["Lit", 2], ["Var", "n"]]])
        self.assertEqual(c[1][2][1], ["Shape", [["Var", "n"]]])

    def test_tuples_and_unpacking(self):
        prog, errors = program(
            """
            def f(x: Float[Tensor, "n"]) -> Float[Tensor, "n"]:
                p, q = g(x)
                return q

            def g(x: Float[Tensor, "n"]) -> tuple[Float[Tensor, "n"], Float[Tensor, "n"]]:
                return x, x
            """
        )
        self.assertEqual(errors, [])
        self.assertEqual(
            prog["functions"][0]["body"][0][3],
            ["Unpack", ["p", "q"], ["Call", "g", [["Var", "x"]]]],
        )

    def test_bitwise_operators(self):
        [a] = body(
            """
            def f(x: Bool[Tensor, "n"], y: Bool[Tensor, "n"]) -> Bool[Tensor, "n"]:
                return ~x & y | x ^ y
            """
        )
        self.assertEqual(a[1][0], "Call")
        self.assertEqual(a[1][1], "operator.or_")
        and_ = a[1][2][0]
        self.assertEqual(and_[1], "operator.and_")
        self.assertEqual(and_[2][0][1], "operator.invert")

    def test_user_calls_bind_keywords(self):
        prog, errors = program(
            """
            def f(x: Float[Tensor, "n"]) -> Float[Tensor, "n"]:
                return g(x, scale=2.0)

            def g(x: Float[Tensor, "*a"], k: int = 3, scale: float = 1.0) -> Float[Tensor, "*a"]:
                return x
            """
        )
        self.assertEqual(errors, [])
        self.assertEqual(
            prog["functions"][0]["body"][0][3][1],
            ["Call", "g", [["Var", "x"], ["Lit", 3], ["Scalar"]]],
        )

    def test_errors(self):
        for src, msg in [
            ("if x: pass", "`if` isn't supported yet"),
            ("for i in x: pass", "`for` isn't supported yet"),
            ("x += 1", "write `x = x \\+ 1`"),
            ("a = b = x", "only assignments to a name"),
            ("a, b.c = x, x", "only assignments to a name"),
            ("print(x)", "an expression statement isn't supported"),
            ("return", "must return a value"),
            ("return y", "`y` isn't a local variable"),
            ("return (*x, x)", "starred items in tuples"),
            ("return x[0]", "indexing isn't supported"),
            ("return x.shape", "`x.shape` isn't supported"),
            ("return x.shape[0]", r"`x.shape\[i\]` isn't supported"),
            ("return x.frob()", "no stub for the method `.frob`"),
            ("return x.sum", "`.sum` is a method; call it"),
            ("return torch.frob(x)", "no stub for `torch.frob`"),
            ("return frob(x)", "unknown function `frob`"),
            ("return x.softmax(axis=0)", "unexpected keyword argument `axis`"),
            ("return x.sum(0, 1, 2, 3)", "too many positional arguments"),
            ("return torch.softmax(x)", "missing argument `dim`"),
            ("return torch.reshape(x, x)", "`shape` takes a tuple of ints"),
            ("return x % 2", "unsupported expression"),
        ]:
            with self.subTest(src):
                _, errors = program(
                    "import torch\n"
                    'def f(x: Float[Tensor, "n"]) -> Float[Tensor, "n"]:\n'
                    f"    {src}\n"
                )
                self.assertRegex(" ".join(errors), msg)

    def test_other_functions_still_translate(self):
        prog, errors = program(
            """
            def bad(x: Float[Tensor, "n"]) -> Float[Tensor, "n"]:
                while True:
                    pass

            def unannotated(x):
                return x

            def good(x: Float[Tensor, "n"]) -> Float[Tensor, "n"]:
                return bad(x)
            """
        )
        self.assertEqual(len(errors), 1)
        # bad's signature is still usable; its body isn't checked
        self.assertEqual(
            [(f["name"], f["body"] is None) for f in prog["functions"]],
            [("bad", True), ("good", False)],
        )


MODULE = """
import torch
import torch.nn as nn
from jaxtyping import Float, Int
from torch import Tensor


class Block(nn.Module):
    def __init__(self, d_model: int, h: int, dropout: float = 0.1, max_len: int = 64):
        super().__init__()
        self.d_k = d_model // h
        self.w = nn.Linear(d_model, 4 * d_model, bias=False)
        self.norm = nn.LayerNorm(d_model)
        self.dropout = nn.Dropout(p=dropout)

    def forward(self, x: Float[Tensor, "b n d_model"]) -> Float[Tensor, "b n d_model"]:
        return self.norm(x)

    def widen(self, x: Float[Tensor, "b d_model"]) -> Float[Tensor, "b 4*d_model"]:
        return self.w(self.dropout(x))

    def heads(self, x: Float[Tensor, "b d_model"]) -> Float[Tensor, "b d_model"]:
        k = self.d_k
        return self(x)


class Outer(nn.Module):
    def __init__(self, d: int):
        super().__init__()
        self.block = Block(2 * d, 4)

    def forward(self, x: Float[Tensor, "b 2*d"]) -> Float[Tensor, "b 8*d"]:
        return self.block.widen(x)
"""


def functions(src):
    prog, errors = program(src)
    return {f["name"]: f for f in prog["functions"]}, errors


def returned(f):
    """The term a one-statement body returns."""
    return [s[3] for s in f["body"] if s[0] == "At"][-1][1]


class Modules(unittest.TestCase):
    def test_method_signature(self):
        fs, errors = functions(MODULE)
        self.assertEqual(errors, [])
        sig = fs["Block.forward"]["sig"]
        # the instance dims come first; self isn't a parameter
        self.assertEqual(
            sig["params"],
            [
                ["d_model", ["Int"]],
                ["h", ["Int"]],
                ["max_len", ["Int"]],
                ["x", ["Array", [["Id", "b"], ["Id", "n"], ["Id", "d_model"]]]],
            ],
        )
        self.assertEqual(sig["exists"], [])  # d_model is the instance's
        self.assertEqual(
            sig["instances"],
            [
                {
                    "init": "Block.__init__",
                    "ints": [["d_model", "d_model"], ["h", "h"], ["max_len", "max_len"]],
                }
            ],
        )

    def test_init(self):
        fs, _ = functions(MODULE)
        init = fs["Block.__init__"]
        self.assertEqual(init["sig"]["ret"], ["Tuple", []])
        self.assertNotIn("instances", init["sig"])
        stmts = [s[3] if s[0] == "At" else s for s in init["body"]]
        # super().__init__() is skipped, and falling off the end returns None
        self.assertEqual(stmts[0][:2], ["Let", "self.d_k"])
        self.assertEqual(
            stmts[1],
            [
                "Let",
                "self.w",
                [
                    "Call",
                    "torch.nn.Linear.__init__",
                    [
                        ["Var", "d_model"],
                        ["Call", "operator.mul", [["Lit", 4], ["Var", "d_model"]]],
                        ["Lit", 0],
                    ],
                ],
            ],
        )
        self.assertEqual(stmts[-1], ["Return", ["Tuple", []]])

    def test_attribute_calls(self):
        fs, _ = functions(MODULE)
        # self.norm = nn.LayerNorm(d_model), so self.norm(x) is its forward
        self.assertEqual(
            returned(fs["Block.forward"]),
            ["Call", "torch.nn.LayerNorm.forward", [["Var", "d_model"], ["Var", "x"]]],
        )
        widen = returned(fs["Block.widen"])
        self.assertEqual(widen[1], "torch.nn.Linear.forward")
        self.assertEqual(widen[2][0], ["Var", "d_model"])
        self.assertEqual(widen[2][2][1], "torch.nn.Dropout.forward")

    def test_self_calls(self):
        fs, _ = functions(MODULE)
        ints = [["Var", "d_model"], ["Var", "h"], ["Var", "max_len"]]
        self.assertEqual(
            returned(fs["Block.heads"]), ["Call", "Block.forward", ints + [["Var", "x"]]]
        )

    def test_int_attributes(self):
        fs, _ = functions(MODULE)
        [let, _] = [s[3] for s in fs["Block.heads"]["body"]]
        # self.d_k is d_model // h again, from the method's instance dims
        self.assertEqual(let[2][1], "operator.floordiv")
        self.assertEqual(let[2][2], [["Var", "d_model"], ["Var", "h"]])

    def test_nested_modules(self):
        fs, _ = functions(MODULE)
        # self.block = Block(2 * d, 4): its dims are terms over Outer's d
        call = returned(fs["Outer.forward"])
        self.assertEqual(call[1], "Block.widen")
        two_d, four, max_len, x = call[2]
        self.assertEqual(two_d[2], [["Lit", 2], ["Var", "d"]])
        self.assertEqual((four, max_len, x), (["Lit", 4], ["Lit", 64], ["Var", "x"]))

    def test_stub_invariant_in_env(self):
        prog, _ = program(MODULE)
        env = {e["name"]: e["overloads"] for e in prog["env"]}
        [lin] = env["torch.nn.Linear.forward"]
        self.assertEqual(lin["instances"][0]["init"], "torch.nn.Linear.__init__")
        # forward's callers assume the constructor's requires
        [init] = env["torch.nn.Linear.__init__"]
        self.assertEqual(init["requires"][0], ["Le", ["Int", 0], ["Id", "in_features"]])

    def test_locals_named_like_instance_dims(self):
        fs, errors = functions(
            MODULE
            + """
class C(nn.Module):
    def __init__(self, d: int):
        super().__init__()
        self.w = nn.Linear(d, d)

    def forward(self, x: Float[Tensor, "b d"]) -> Float[Tensor, "b d"]:
        d = x
        return self.w(d)
"""
        )
        self.assertEqual(errors, [])
        let, ret = [s[3] for s in fs["C.forward"]["body"]]
        self.assertEqual(let, ["Let", "d'", ["Var", "x"]])
        self.assertEqual(ret[1][2], [["Var", "d"], ["Var", "d"], ["Var", "d'"]])

    def test_errors(self):
        for body, msg in [
            ("self.v = x\n        return x", "attributes can only be assigned in `__init__`"),
            ("return self.nope(x)", "`C` has no method `nope`"),
            ("return self.missing", "no attribute `missing` assigned in __init__"),
            ("return self", "`self` can only be used for its attributes and methods"),
            ("return torch.relu(self.w)", "`self.w` is a module; only calls to it are supported"),
            ("return self.forward", "`self.forward` is a method; call it"),
            ("return self.twice(x)", "`self.twice` must be assigned once"),
            ("return self.local(x)", "`e` isn't an int parameter of `__init__`"),
            ("return self.k(x)", "`self.k` isn't a module"),
            ("m = nn.Linear(d, d)\n        return x", "a module can only be created in `__init__`"),
        ]:
            with self.subTest(body):
                _, errors = program(
                    f"""
import torch
import torch.nn as nn

class C(nn.Module):
    def __init__(self, d: int):
        super().__init__()
        self.w = nn.Linear(d, d)
        self.k = d + 1
        self.twice = nn.Linear(d, d)
        self.twice = nn.Linear(d, d)
        e = 2 * d
        self.local = nn.Linear(d, e)

    def forward(self, x: Float[Tensor, "n d"]) -> Float[Tensor, "n d"]:
        {body}
"""
                )
                self.assertRegex(" ".join(errors), msg)

    def test_init_errors(self):
        for body, msg in [
            ("d = 2 * d", "can't reassign `d`: it's one of the instance's dims"),
            ("self.w.weight = None", "assigning to an attribute of a module"),
            ("self.a = self.b\n        self.b = self.a", "`self.b` is defined in terms of itself"),
        ]:
            with self.subTest(body):
                _, errors = program(
                    f"""
import torch.nn as nn

class C(nn.Module):
    def __init__(self, d: int):
        super().__init__()
        self.w = nn.Linear(d, d)
        {body}
"""
                )
                self.assertRegex(" ".join(errors), msg)

    def test_signature_errors(self):
        _, errors = program(
            """
import torch.nn as nn

class C(nn.Module):
    def __init__(self, n: int):
        super().__init__()

    def forward(self, x: Float[Tensor, "n"], n: int) -> Float[Tensor, "n"]:
        return x

    @staticmethod
    def helper(x: Float[Tensor, "n"]) -> Float[Tensor, "n"]:
        return x
"""
        )
        self.assertRegex(errors[0], "decorated methods aren't supported")
        self.assertRegex(errors[1], "parameter n has the name of one of the instance's dims")

    def test_other_classes_are_skipped(self):
        prog, t = translate("class C:\n    def f(self, x: int) -> int:\n        return x\n", STUBS)
        self.assertEqual(prog["functions"], [])
        self.assertIn("only classes that subclass nn.Module are checked", t.notes[0].message)

    def test_stub_module_classes(self):
        stubs = Stubs()
        load_stub_module(
            stubs,
            "nn",
            ast.parse(
                textwrap.dedent(
                    """
                    class Linear(Module):
                        def __init__(self, i: int, o: int, bias: bool = True) -> None:
                            assert i >= 0
                        def forward(self, x: Float[Tensor, "*B i"]) -> Float[Tensor, "*B o"]: ...
                    """
                )
            ),
        )
        self.assertEqual(stubs.classes["nn.Linear"].ints, ["i", "o"])  # not the bool
        [fwd] = stubs.functions["nn.Linear.forward"].overloads
        self.assertEqual([p[0] for p in fwd.sig["params"]], ["i", "o", "x"])
        self.assertEqual([p.name for p in fwd.params], ["x"])  # what callers pass
        [init] = stubs.functions["nn.Linear.__init__"].overloads
        self.assertEqual(init.sig["requires"], [["Le", ["Int", 0], ["Id", "i"]]])
        self.assertEqual(init.sig["ret"], ["Tuple", []])
        self.assertNotIn("forward", stubs.methods)  # only called on instances


OPTIONAL = """
import torch
import torch.nn as nn
from typing import Optional, Union
from jaxtyping import Bool, Float
from torch import Tensor


def attend(
    x: Float[Tensor, "*b q m"],
    mask: Optional[Bool[Tensor, "*#b #q m"]] = None,
    dropout: Optional[nn.Dropout] = None,
) -> Float[Tensor, "*b q m"]:
    if mask is not None:
        x = x.masked_fill(mask, -1e9)
    p = x.softmax(dim=-1)
    if dropout is None:
        return p
    return dropout(p)


def caller(x: Float[Tensor, "b q m"], m: Bool[Tensor, "b 1 m"]) -> Float[Tensor, "b q m"]:
    y = attend(x)
    z = attend(y, m)
    return attend(z, mask=None)


class Block(nn.Module):
    def __init__(self, d: int):
        super().__init__()
        self.dropout = nn.Dropout(0.1)
        self.w = nn.Linear(d, d)

    def forward(
        self, x: Float[Tensor, "b q q"], mask: Union[Bool[Tensor, "b q q"], None] = None
    ) -> Float[Tensor, "b q q"]:
        # passing an Optional on, and a module argument
        return attend(x, mask, dropout=self.dropout)
"""


def body_of(f):
    return [s[3] for s in f["body"]]


class Optionals(unittest.TestCase):
    def test_a_variant_per_choice_of_nones(self):
        fs, errors = functions(OPTIONAL)
        self.assertEqual(errors, [])
        self.assertEqual(
            [n for n in fs if n.startswith("attend")],
            [
                "attend",
                "attend[mask=None]",
                "attend[dropout=None]",
                "attend[mask=None,dropout=None]",
            ],
        )
        # a None parameter isn't in the signature, and Dropout has no dims
        self.assertEqual([p[0] for p in fs["attend"]["sig"]["params"]], ["x", "mask"])
        self.assertEqual([p[0] for p in fs["attend[mask=None]"]["sig"]["params"]], ["x"])
        self.assertEqual(
            fs["attend"]["sig"]["params"][1][1],
            ["Array", [["Broadcast", "b"], ["BroadcastDim", "q"], ["Id", "m"]]],
        )

    def test_ifs_on_none_are_decided(self):
        fs, _ = functions(OPTIONAL)
        full = body_of(fs["attend"])
        self.assertEqual(full[0][:2], ["Let", "x"])  # the masked_fill branch
        self.assertEqual(full[-1][1][1], "torch.nn.Dropout.forward")
        # no mask, no dropout: neither branch, and the early return
        self.assertEqual(
            body_of(fs["attend[mask=None,dropout=None]"]),
            [
                ["Let", "p", ["Call", "torch.Tensor.softmax", [["Var", "x"], ["Lit", -1]]]],
                ["Return", ["Var", "p"]],
            ],
        )

    def test_calls_select_a_variant(self):
        fs, _ = functions(OPTIONAL)
        y, z, ret = body_of(fs["caller"])
        self.assertEqual(y[2], ["Call", "attend[mask=None,dropout=None]", [["Var", "x"]]])
        # m is also a dim, so the parameter is m'
        self.assertEqual(z[2], ["Call", "attend[dropout=None]", [["Var", "y"], ["Var", "m'"]]])
        self.assertEqual(ret[1][1], "attend[mask=None,dropout=None]")

    def test_optionals_are_passed_on(self):
        fs, _ = functions(OPTIONAL)
        # Union[T, None] is Optional[T]; the module argument has no dims
        self.assertEqual(
            returned(fs["Block.forward"]),
            ["Call", "attend", [["Var", "x"], ["Var", "mask"]]],
        )
        self.assertEqual(
            returned(fs["Block.forward[mask=None]"]), ["Call", "attend[mask=None]", [["Var", "x"]]]
        )

    def test_pep_604(self):
        fs, errors = functions(
            """
            def f(x: Float[Tensor, "n"], y: Float[Tensor, "n"] | None = None) -> Float[Tensor, "n"]:
                return x if y is None else x + y
            """
        )
        self.assertEqual(errors, [])
        self.assertEqual(returned(fs["f[y=None]"]), ["Var", "x"])
        self.assertEqual(returned(fs["f"])[1], "operator.add")

    def test_none_locals(self):
        fs, errors = functions(
            """
            def f(x: Float[Tensor, "n"]) -> Float[Tensor, "n"]:
                y = None
                if y is None and not (x is None):
                    y = x
                return y
            """
        )
        self.assertEqual(errors, [])
        self.assertEqual(body_of(fs["f"]), [["Let", "y", ["Var", "x"]], ["Return", ["Var", "y"]]])

    def test_module_parameters(self):
        fs, errors = functions(
            """
            import torch.nn as nn

            def f(lin: nn.Linear, x: Float[Tensor, "b i"]) -> Float[Tensor, "b o"]:
                return lin(x)
            """
        )
        self.assertEqual(errors, [])
        sig = fs["f"]["sig"]
        # an instance is its dims, and satisfies its constructor's requires
        self.assertEqual(
            [p[0] for p in sig["params"]], ["lin.in_features", "lin.out_features", "x"]
        )
        self.assertEqual(
            sig["instances"],
            [
                {
                    "init": "torch.nn.Linear.__init__",
                    "ints": [
                        ["in_features", "lin.in_features"],
                        ["out_features", "lin.out_features"],
                    ],
                }
            ],
        )
        self.assertEqual(
            returned(fs["f"]),
            [
                "Call",
                "torch.nn.Linear.forward",
                [["Var", "lin.in_features"], ["Var", "lin.out_features"], ["Var", "x"]],
            ],
        )

    def test_errors(self):
        for src, msg in [
            ("return mask", "`mask` is None here"),
            ("return g(x, None)", "`y` can't be None"),
            ("return attend(x, dropout=x)", "`dropout` takes a `torch.nn.Dropout` module"),
            (
                "if x.sum() > 0:\n        return x\n    return x",
                "`if` isn't supported yet, except on whether",
            ),
            ("return x if x.sum() else x", "a conditional expression isn't supported yet"),
            ("return x if z is None else x", "`z` isn't a local variable"),
        ]:
            with self.subTest(src):
                _, errors = program(
                    OPTIONAL
                    + "\n"
                    + "def g(x: Float[Tensor, 'n'], y: Float[Tensor, 'n']) -> Float[Tensor, 'n']:\n"
                    + "    return x\n"
                    + "def f(\n"
                    + "    x: Float[Tensor, 'n'], mask: Optional[Float[Tensor, 'n']] = None\n"
                    + ") -> Float[Tensor, 'n']:\n"
                    + f"    {src}\n"
                )
                self.assertRegex(" ".join(errors), msg)

    def test_errors_are_reported_once(self):
        _, errors = program(
            """
            def f(
                x: Float[Tensor, "n"], a: Optional[int] = None, b: Optional[int] = None
            ) -> Float[Tensor, "n"]:
                return x[0]
            """
        )
        self.assertEqual(len(errors), 1)

    def test_signature_errors(self):
        five = ", ".join(f"a{i}: Optional[int] = None" for i in range(5))
        for src, msg in [
            (
                f'def f(x: Float[Tensor, "n"], {five}) -> int: return 0',
                "at most 4 parameters can be Optional",
            ),
            (
                "class C(nn.Module):\n"
                "    def __init__(self, d: Optional[int] = None):\n"
                "        super().__init__()",
                "`Optional` parameters aren't supported on `__init__`",
            ),
            (
                "def f(x: nn.Dropout = 3) -> int: return 0",
                "is a module; its default can only be None",
            ),
        ]:
            with self.subTest(src):
                _, errors = program("import torch.nn as nn\nfrom typing import Optional\n" + src)
                self.assertRegex(" ".join(errors), msg)


class Asserts(unittest.TestCase):
    def test_divisibility(self):
        [assume, ret] = body(
            """
            def f(x: Float[Tensor, "n"], a: int, b: int) -> Float[Tensor, "n"]:
                assert a % b == 0
                return x
            """
        )
        # b * (a // b) == a
        self.assertEqual(
            assume,
            [
                "Assume",
                ["Eq", ["Mul", ["Id", "b"], ["Div", ["Id", "a"], ["Id", "b"]]], ["Id", "a"]],
            ],
        )

    def test_comparisons(self):
        stmts = body(
            """
            def f(x: Float[Tensor, "n"], a: int, b: int) -> Float[Tensor, "n"]:
                assert 0 < a <= b + 1 and a != b and a > -b
                return x
            """
        )
        # != can't be stated, so that conjunct is dropped
        self.assertEqual(
            [s[1] for s in stmts[:-1]],
            [
                ["Lt", ["Int", 0], ["Id", "a"]],
                ["Le", ["Id", "a"], ["Add", ["Id", "b"], ["Int", 1]]],
                ["Lt", ["Sub", ["Int", 0], ["Id", "b"]], ["Id", "a"]],
            ],
        )

    def test_dropped(self):
        for test in [
            "x is not None",
            "a > 0.5",
            "isinstance(x, Tensor)",
            "x.shape[0] == a",
            "a % 2 == 1",
        ]:
            with self.subTest(test):
                self.assertEqual(
                    body(
                        f"""
                        def f(x: Float[Tensor, "n"], a: int) -> Float[Tensor, "n"]:
                            assert {test}
                            return x
                        """
                    ),
                    [["Return", ["Var", "x"]]],
                )

    def test_init_asserts_are_ensures(self):
        fs, errors = functions(
            """
            import torch.nn as nn

            class C(nn.Module):
                def __init__(self, d: int, h: int):
                    super().__init__()
                    assert d % h == 0 and d % (h - 1) == 0
                    assert d >= 2 * h
            """
        )
        self.assertEqual(errors, [])
        # facts about the instance dims, whose divisors can be inferred positive
        self.assertEqual(
            fs["C.__init__"]["sig"]["ensures"],
            [
                ["Eq", ["Mul", ["Id", "h"], ["Div", ["Id", "d"], ["Id", "h"]]], ["Id", "d"]],
                ["Le", ["Mul", ["Int", 2], ["Id", "h"]], ["Id", "d"]],
            ],
        )


if __name__ == "__main__":
    unittest.main()
