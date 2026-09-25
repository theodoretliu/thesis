"""End-to-end: every examples/pass file checks, and every examples/fail file
fails with each of its `# expect-error: <substring>` lines in the output.
Needs the checker built (dune build in checker/)."""

import re
import subprocess
import unittest
from pathlib import Path

from shapecheck.cli import DEFAULT_STUBS, check_file, checker_path
from shapecheck.translate import load_stubs

EXAMPLES = Path(__file__).resolve().parents[2] / "examples"
STUBS = load_stubs([DEFAULT_STUBS])


class Examples(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.checker = checker_path()

    def test_pass(self):
        files = sorted((EXAMPLES / "pass").glob("*.py"))
        self.assertTrue(files)
        for path in files:
            with self.subTest(path.name):
                report = check_file(path, STUBS, self.checker)
                self.assertEqual(report.errors, [])
                self.assertGreater(report.checked, 0)

    def test_fail(self):
        files = sorted((EXAMPLES / "fail").glob("*.py"))
        self.assertTrue(files)
        for path in files:
            with self.subTest(path.name):
                expected = re.findall(r"# expect-error: (.*)", path.read_text())
                # so a file can't pass for the wrong reason
                self.assertTrue(expected, "no # expect-error line")
                report = check_file(path, STUBS, self.checker)
                self.assertTrue(report.errors, "no errors")
                output = "\n".join(report.errors)
                for e in expected:
                    self.assertIn(e.strip(), output)

    def test_inferred_preconditions(self):
        report = check_file(EXAMPLES / "pass" / "sizes.py", STUBS, self.checker)
        self.assertEqual(
            report.inferred,
            {
                "causal_mask": ["size >= 0"],
                "split_heads": ["b >= 1"],
                "token_loss": ["v >= 1"],
            },
        )
        self.assertIn("causal_mask requires size >= 0 (inferred from its body)", report.notes[0])

    def test_module_invariants(self):
        # constructors infer what nn.Linear and nn.Embedding require. every
        # instance satisfies it, so the methods assume it rather than
        # inferring requires of their own
        report = check_file(EXAMPLES / "pass" / "modules.py", STUBS, self.checker)
        self.assertEqual(
            report.inferred,
            {
                "FeedForward.__init__": ["d_model >= 0", "expansion >= 0"],
                "Classifier.__init__": ["vocab >= 0", "d_model >= 0", "n_classes >= 0"],
            },
        )
        self.assertEqual(
            sorted(report.passed),
            [
                "Classifier.__init__",
                "Classifier.features",
                "Classifier.forward",
                "FeedForward.__init__",
                "FeedForward.forward",
            ],
        )


class CheckerCli(unittest.TestCase):
    def test_invalid_ir(self):
        proc = subprocess.run(
            [str(checker_path())], input='{"env": []}', capture_output=True, text=True
        )
        self.assertEqual(proc.returncode, 2)
        self.assertIn("missing field functions", proc.stderr)

    def test_stub_kind_errors_are_reported(self):
        program = (
            '{"env": [{"name": "bad", "overloads": [{"params": [], '
            '"ret": ["Array", [["Id", "n"]]], "requires": [], "exists": [], "ensures": []}]}], '
            '"functions": []}'
        )
        proc = subprocess.run([str(checker_path())], input=program, capture_output=True, text=True)
        self.assertEqual(proc.returncode, 1)
        self.assertIn("in stub bad", proc.stdout)


if __name__ == "__main__":
    unittest.main()
