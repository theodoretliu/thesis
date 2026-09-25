"""The goal: check the Transformer of "Attention Is All You Need"
(examples/goal/transformer.py). See docs/12-transformer-goal.md.

test_goal is the failing spec: it's an expected failure until every
annotated function and method in the file checks. test_progress is a ratchet
over PASSING, so progress can't silently regress, and a newly passing
function has to be recorded."""

import ast
import unittest
from pathlib import Path

from shapecheck.cli import DEFAULT_STUBS, check_file, checker_path
from shapecheck.translate import annotated, load_stubs

GOAL = Path(__file__).resolve().parents[2] / "examples" / "goal" / "transformer.py"

# the functions and methods that check today; add to it as gaps close
PASSING = {
    "subsequent_mask",
    "make_masks",
    "loss",
    "MultiHeadedAttention.__init__",
    "PositionwiseFeedForward.__init__",
    "PositionwiseFeedForward.forward",
    "Embeddings.__init__",
    "Embeddings.forward",
    "EncoderLayer.__init__",
    "DecoderLayer.__init__",
    "Transformer.encode",
    "Transformer.decode",
    "Transformer.forward",
}


def targets() -> list[str]:
    """Every annotated function and method, as f or Class.method."""
    names = []
    for node in ast.parse(GOAL.read_text()).body:
        if isinstance(node, ast.FunctionDef) and annotated(node):
            names.append(node.name)
        elif isinstance(node, ast.ClassDef):
            names += [
                f"{node.name}.{m.name}"
                for m in node.body
                if isinstance(m, ast.FunctionDef) and annotated(m)
            ]
    return names


def passing() -> set[str]:
    report = check_file(GOAL, load_stubs([DEFAULT_STUBS]), checker_path())
    return set(report.passed)


class TransformerGoal(unittest.TestCase):
    def test_targets(self):
        # 4 functions, __init__ and forward of 8 modules, and Transformer's
        # __init__, encode, decode and forward
        self.assertEqual(len(targets()), 24)
        self.assertLessEqual(PASSING, set(targets()))

    def test_progress(self):
        now = passing()
        self.assertFalse(PASSING - now, "these used to check and no longer do")
        self.assertFalse(now - PASSING, "these check now: add them to PASSING")

    @unittest.expectedFailure
    def test_goal(self):
        missing = sorted(set(targets()) - passing())
        self.assertEqual(missing, [], f"{len(missing)} of {len(targets())} don't check yet")


if __name__ == "__main__":
    unittest.main()
