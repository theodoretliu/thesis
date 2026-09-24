"""Run transformer.py under jaxtyping's runtime checker, to show the goal is
correct code: a forward pass and the loss, at two sizes.

    uvx --python 3.12 --with torch --with numpy --with jaxtyping --with beartype \
        python examples/goal/run_transformer.py
"""

import sys
from pathlib import Path

import torch
from jaxtyping import TypeCheckError, install_import_hook

sys.path.insert(0, str(Path(__file__).parent))
with install_import_hook("transformer", "beartype.beartype"):
    import transformer as T


def step(vocab: int, n_layers: int, d_model: int, d_ff: int, h: int, b: int, s: int, t: int):
    model = T.Transformer(vocab, n_layers, d_model, d_ff, h, dropout=0.1)
    src = torch.randint(1, vocab, (b, s))
    tgt = torch.randint(1, vocab, (b, t))
    src[0, -1] = 0  # some padding
    src_mask, tgt_mask = T.make_masks(src, tgt, pad=0)
    logits = model(src, tgt, src_mask, tgt_mask)
    assert logits.shape == (b, t, vocab)
    loss = T.loss(logits, tgt, pad=0)
    loss.backward()
    return loss.item()


print("small:", step(vocab=11, n_layers=2, d_model=16, d_ff=32, h=4, b=3, s=7, t=5))
print("paper base, 1 layer:", step(vocab=100, n_layers=1, d_model=512, d_ff=2048, h=8, b=2, s=9, t=4))

# the runtime checker is on: a mask for the wrong source length is caught
model = T.Transformer(11, 1, 16, 32, 4)
src, tgt = torch.randint(1, 11, (2, 7)), torch.randint(1, 11, (2, 5))
src_mask, tgt_mask = T.make_masks(src, tgt, pad=0)
try:
    model(src, tgt, src_mask[:, :, :6], tgt_mask)
    raise SystemExit("expected a runtime shape error")
except TypeCheckError:
    print("runtime checking is active")
