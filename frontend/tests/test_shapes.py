import unittest

from shapecheck.shapes import Scope, ShapeError, parse_shape


def user(s, binding=True, scope=None):
    return parse_shape(s, scope or Scope(stub=False), binding)


def stub(s, binding=True, scope=None):
    return parse_shape(s, scope or Scope(stub=True), binding)


class JaxtypingSyntax(unittest.TestCase):
    def test_named_and_fixed_dims(self):
        self.assertEqual(user("batch 3 d"), [["Id", "batch"], ["Int", 3], ["Id", "d"]])

    def test_scalar(self):
        self.assertEqual(user(""), [])

    def test_variadic(self):
        self.assertEqual(user("*batch d"), [["Spread", "batch"], ["Id", "d"]])

    def test_anonymous(self):
        self.assertEqual(
            user("... _ _foo *_"),
            [["Spread", "_1"], ["Id", "_2"], ["Id", "_3"], ["Spread", "_4"]],
        )

    def test_broadcastable_variadic(self):
        scope = Scope(stub=False)
        self.assertEqual(user("*#b n", scope=scope), [["Spread", "b"], ["Id", "n"]])
        self.assertEqual(user("*#b", scope=scope), [["Broadcast", "b"]])
        self.assertEqual(user("#*b", scope=scope), [["Broadcast", "b"]])
        self.assertEqual(user("*b", scope=scope), [["Spread", "b"]])

    def test_arithmetic(self):
        self.assertEqual(
            user("n n-1 2*n (n+1)//2"),
            [
                ["Id", "n"],
                ["Sub", ["Id", "n"], ["Int", 1]],
                ["Mul", ["Int", 2], ["Id", "n"]],
                ["Div", ["Add", ["Id", "n"], ["Int", 1]], ["Int", 2]],
            ],
        )

    def test_return_type_binds_no_spreads(self):
        scope = Scope(stub=False)
        user("*b n", scope=scope)
        self.assertEqual(user("*b m", binding=False, scope=scope), [["Spread", "b"], ["Id", "m"]])
        with self.assertRaisesRegex(ShapeError, "isn't bound by a parameter"):
            user("*c", binding=False, scope=scope)
        with self.assertRaisesRegex(ShapeError, "unnamed run of dims"):
            user("...", binding=False, scope=scope)

    def test_rejected(self):
        for s, msg in [
            ("#n", "single broadcastable dim"),
            ("?n", "`\\?` dims"),
            ("n/2", "use //"),
            ("*drop(A,0)", "only available in stubs"),
            ("n *n", "both as a dim and as"),
            ("f(n)", "unsupported dim expression"),
            ("(n", "unbalanced"),
        ]:
            with self.subTest(s):
                with self.assertRaisesRegex(ShapeError, msg):
                    user(s)


class StubSyntax(unittest.TestCase):
    def test_list_functions(self):
        scope = Scope(stub=True)
        stub("*A *B", scope=scope)
        self.assertEqual(
            stub("*drop(A, dim) *keep(A,0,-1) *permute(A,1,0)", scope=scope),
            [["Drop", "A", ["dim"]], ["Keep", "A", [0, -1]], ["Permute", "A", [1, 0]]],
        )
        self.assertEqual(
            stub("*setat(A,dim,1) *insertat(A,-1,n+1) *broadcast(A,B)", scope=scope),
            [
                ["SetAt", "A", ["dim"], ["Int", 1]],
                ["InsertAt", "A", -1, ["Add", ["Id", "n"], ["Int", 1]]],
                ["Broadcasted", ["A", "B"]],
            ],
        )

    def test_prod_and_rank(self):
        self.assertEqual(
            stub("b prod(A) rank(A)+1"),
            [["Id", "b"], ["Prod", "A"], ["Add", ["Rank", "A"], ["Int", 1]]],
        )

    def test_unknown_list_function(self):
        with self.assertRaisesRegex(ShapeError, "unknown list function"):
            stub("*frob(A)")


if __name__ == "__main__":
    unittest.main()
