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
            def f(n: Float[Tensor, "n"], d: int) -> Float[Tensor, "d"]:
                return n
            """
        )
        self.assertEqual([p[0] for p in f["sig"]["params"]], ["n'", "d'"])
        self.assertEqual(f["sig"]["exists"], ["d"])
        self.assertEqual(f["body"][0][3], ["Return", ["Var", "n'"]])

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
            ('def f(x: Float[Tensor, "#n"]) -> int: return 0', "single broadcastable dim"),
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
                '''docstrings and asserts are skipped'''
                assert x.ndim == 1
                y = x
                z: Float[Tensor, "n"] = y
                return z
            """
        )
        self.assertEqual(
            f["body"],
            [
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
        self.assertEqual(a[2][2], [["Shape", [["Var", "n'"], ["Lit", 2]]]])
        self.assertEqual(b[2][2][1], ["Shape", [["Lit", 2], ["Var", "n'"]]])
        self.assertEqual(c[1][2][1], ["Shape", [["Var", "n'"]]])

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
            ("a, b = x, x", "single name"),
            ("print(x)", "an expression statement isn't supported"),
            ("return", "must return a value"),
            ("return y", "`y` isn't a local variable"),
            ("return (x, x)", "tuples are only supported as shapes"),
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


if __name__ == "__main__":
    unittest.main()
