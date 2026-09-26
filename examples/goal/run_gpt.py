"""Run gpt.py under jaxtyping's runtime checker, to show the goal is correct
code: training steps and generation at two sizes, through both attention
paths (flash, and the manual one with a causal mask buffer).

    uvx --python 3.12 --with torch --with numpy --with jaxtyping --with beartype \
        python examples/goal/run_gpt.py
"""

import sys
from pathlib import Path

import torch
import torch.nn.functional as F
from jaxtyping import TypeCheckError, install_import_hook

sys.path.insert(0, str(Path(__file__).parent))
with install_import_hook("gpt", "beartype.beartype"):
    import gpt as G


def build(config: G.GPTConfig, flash: bool) -> G.GPT:
    """A model on the flash path, or on the manual path as on PyTorch < 2.0."""
    if flash:
        return G.GPT(config)
    sdpa = F.scaled_dot_product_attention
    del F.scaled_dot_product_attention
    try:
        return G.GPT(config)
    finally:
        F.scaled_dot_product_attention = sdpa


def step(config: G.GPTConfig, flash: bool, b: int, t: int, top_k):
    model = build(config, flash)
    assert all(block.attn.flash == flash for block in model.transformer.h)
    idx = torch.randint(0, config.vocab_size, (b, t))
    targets = torch.randint(0, config.vocab_size, (b, t))
    logits, loss = model(idx, targets)
    assert logits.shape == (b, t, config.vocab_size)
    loss.backward()
    train_loss = loss.item()
    model.eval()
    logits, loss = model(idx)
    assert logits.shape == (b, 1, config.vocab_size) and loss is None
    # past block_size, generate crops the context
    out = model.generate(idx, max_new_tokens=config.block_size, top_k=top_k)
    assert out.shape == (b, t + config.block_size)
    return train_loss


small = G.GPTConfig(block_size=8, vocab_size=11, n_layer=2, n_head=4, n_embd=16, bias=False)
print("small, flash:", step(small, flash=True, b=3, t=5, top_k=None))
print("small, manual:", step(small, flash=False, b=3, t=8, top_k=3))
# GPT-2 small's widths, with fewer layers and a shorter context
gpt2 = G.GPTConfig(block_size=16, vocab_size=50304, n_layer=1, n_head=12, n_embd=768, dropout=0.1)
print("gpt2 widths, flash:", step(gpt2, flash=True, b=2, t=6, top_k=10))
print("gpt2 widths, manual:", step(gpt2, flash=False, b=2, t=6, top_k=None))

# the runtime checker is on: targets for the wrong sequence length are caught
model = G.GPT(small)
try:
    model(torch.randint(0, 11, (2, 5)), torch.randint(0, 11, (2, 4)))
    raise SystemExit("expected a runtime shape error")
except TypeCheckError:
    print("runtime checking is active")
