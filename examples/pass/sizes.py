import torch
import torch.nn.functional as F
from jaxtyping import Bool, Float, Int
from torch import Tensor


def causal_mask(size: int) -> Bool[Tensor, "size size"]:
    # a dim named after an int parameter is its value. torch.ones(-1) raises,
    # so the checker infers that callers pass size >= 0
    return torch.tril(torch.ones(size, size)) == 1


def masks(
    tokens: Int[Tensor, "b t"], pad: int
) -> tuple[Bool[Tensor, "b t"], Bool[Tensor, "b t t"]]:
    keep = tokens != pad
    return keep, keep.unsqueeze(-2) & causal_mask(tokens.size(-1))


def padding(tokens: Int[Tensor, "b t"], pad: int) -> Bool[Tensor, "b t"]:
    keep, _ = masks(tokens, pad)
    return keep


def split_heads(x: Float[Tensor, "b n 12"]) -> Float[Tensor, "b n 3 4"]:
    # -1 is n, if b isn't 0: torch can't infer -1 when the other sizes'
    # product is 0, so the checker infers that callers pass b >= 1
    return x.view(x.size(0), -1, 3, 4)


def token_loss(logits: Float[Tensor, "b t v"], target: Int[Tensor, "b t"]) -> Float[Tensor, ""]:
    return F.cross_entropy(logits.reshape(-1, logits.size(-1)), target.reshape(-1))
