# Shapes of torch.nn.functional; see ../__init__.pyi for the stub syntax.

from typing import Optional, overload

from jaxtyping import Float, Shaped
from shapecheck.stubs import Shape
from torch import Tensor

def relu(input: Shaped[Tensor, "*A"]) -> Shaped[Tensor, "*A"]: ...
def gelu(input: Shaped[Tensor, "*A"]) -> Shaped[Tensor, "*A"]: ...
def silu(input: Shaped[Tensor, "*A"]) -> Shaped[Tensor, "*A"]: ...
def sigmoid(input: Shaped[Tensor, "*A"]) -> Shaped[Tensor, "*A"]: ...
def tanh(input: Shaped[Tensor, "*A"]) -> Shaped[Tensor, "*A"]: ...
def softmax(input: Shaped[Tensor, "*A"], dim: int) -> Shaped[Tensor, "*A"]: ...
def log_softmax(input: Shaped[Tensor, "*A"], dim: int) -> Shaped[Tensor, "*A"]: ...
# normalized_shape is one dim, the last: torch allows several
def layer_norm(
    input: Float[Tensor, "*B n"],
    normalized_shape: Shape["n"],
    weight: Optional[Float[Tensor, "n"]] = None,
    bias: Optional[Float[Tensor, "n"]] = None,
    eps: float = 1e-5,
) -> Float[Tensor, "*B n"]: ...
def dropout(input: Shaped[Tensor, "*A"], p: float = 0.5, training: bool = True) -> Shaped[Tensor, "*A"]: ...
# softmax(q @ k.mT / sqrt(e) + mask) @ v, where the mask broadcasts to the
# scores. torch also broadcasts the batch dims of q, k and v, which are equal here
def scaled_dot_product_attention(
    query: Float[Tensor, "*B l e"],
    key: Float[Tensor, "*B s e"],
    value: Float[Tensor, "*B s ev"],
    attn_mask: Optional[Shaped[Tensor, "*#B #l #s"]] = None,
    dropout_p: float = 0.0,
    is_causal: bool = False,
) -> Float[Tensor, "*B l ev"]: ...

# weight is [out_features, in_features]
@overload
def linear(input: Shaped[Tensor, "*A k"], weight: Shaped[Tensor, "p k"]) -> Shaped[Tensor, "*A p"]: ...
@overload
def linear(input: Shaped[Tensor, "*A k"], weight: Shaped[Tensor, "p k"], bias: Shaped[Tensor, "p"]) -> Shaped[Tensor, "*A p"]: ...

# the kernel must fit in the padded image; stride must be positive
@overload
def conv2d(
    input: Shaped[Tensor, "n c h w"],
    weight: Shaped[Tensor, "o c kh kw"],
    *,
    stride: int = 1,
    padding: int = 0,
) -> Shaped[Tensor, "n o (h+2*padding-kh)//stride+1 (w+2*padding-kw)//stride+1"]:
    assert kh <= h + 2 * padding and kw <= w + 2 * padding
@overload
def conv2d(
    input: Shaped[Tensor, "n c h w"],
    weight: Shaped[Tensor, "o c kh kw"],
    bias: Shaped[Tensor, "o"],
    stride: int = 1,
    padding: int = 0,
) -> Shaped[Tensor, "n o (h+2*padding-kh)//stride+1 (w+2*padding-kw)//stride+1"]:
    assert kh <= h + 2 * padding and kw <= w + 2 * padding

# class indices as targets, reduced to a scalar. keyword-only, so an unstubbed
# positional argument like weight is rejected
@overload
def cross_entropy(
    input: Shaped[Tensor, "c"],
    target: Shaped[Tensor, ""],
    *,
    ignore_index: int = -100,
    label_smoothing: float = 0.0,
) -> Shaped[Tensor, ""]: ...
@overload
def cross_entropy(
    input: Shaped[Tensor, "n c *D"],
    target: Shaped[Tensor, "n *D"],
    *,
    ignore_index: int = -100,
    label_smoothing: float = 0.0,
) -> Shaped[Tensor, ""]: ...

# stride defaults to the kernel size
def max_pool2d(input: Shaped[Tensor, "*B h w"], kernel_size: int) -> Shaped[Tensor, "*B h//kernel_size w//kernel_size"]: ...
def avg_pool2d(input: Shaped[Tensor, "*B h w"], kernel_size: int) -> Shaped[Tensor, "*B h//kernel_size w//kernel_size"]: ...
