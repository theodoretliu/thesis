"""The second goal: check nanoGPT (examples/goal/gpt.py). See
docs/17-nanogpt-goal.md.

test_goal is the spec: every annotated function and method in the file
checks. It's an expected failure until the last milestone meets it.
test_progress is a ratchet over PASSING, so a function can't silently stop
checking."""

import unittest
from pathlib import Path

from tests.test_goal import passing, targets

GOAL = Path(__file__).resolve().parents[2] / "examples" / "goal" / "gpt.py"

# the functions and methods that check so far
PASSING = {
    "MLP.__init__",
    "MLP.forward",
    "Block.__init__",
    "Block.forward",
}


class NanoGPTGoal(unittest.TestCase):
    def test_targets(self):
        # __init__ and forward of LayerNorm, CausalSelfAttention, MLP and
        # Block, and GPT's __init__, forward and generate
        self.assertEqual(len(targets(GOAL)), 11)
        self.assertLessEqual(PASSING, set(targets(GOAL)))

    def test_progress(self):
        now = passing(GOAL)
        self.assertFalse(PASSING - now, "these used to check and no longer do")
        self.assertFalse(now - PASSING, "these check now: add them to PASSING")

    @unittest.expectedFailure
    def test_goal(self):
        missing = sorted(set(targets(GOAL)) - passing(GOAL))
        self.assertEqual(missing, [], f"{len(missing)} of {len(targets(GOAL))} don't check yet")


if __name__ == "__main__":
    unittest.main()
