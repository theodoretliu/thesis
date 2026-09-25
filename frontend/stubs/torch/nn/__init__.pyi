# Shapes of torch.nn's modules; see ../__init__.pyi for the stub syntax.
#
# A module class subclasses Module. Its instance dims are the int
# parameters of __init__: a dim in a method's annotation named after one is
# that int, so Linear's forward maps "*B in_features" to "*B out_features".
# Asserts in __init__ are preconditions of the constructor, and every
# instance satisfies them, so its methods may assume them.

from jaxtyping import Float, Int, Shaped
from torch import Tensor

class Module: ...

class Linear(Module):
    def __init__(self, in_features: int, out_features: int, bias: bool = True) -> None:
        assert in_features >= 0 and out_features >= 0
    def forward(self, input: Float[Tensor, "*B in_features"]) -> Float[Tensor, "*B out_features"]: ...

class Embedding(Module):
    def __init__(self, num_embeddings: int, embedding_dim: int) -> None:
        assert num_embeddings >= 0 and embedding_dim >= 0
    def forward(self, input: Int[Tensor, "*A"]) -> Float[Tensor, "*A embedding_dim"]: ...

# normalized_shape as an int: the last dim
class LayerNorm(Module):
    def __init__(self, normalized_shape: int, eps: float = 1e-5, elementwise_affine: bool = True, bias: bool = True) -> None:
        assert normalized_shape >= 0
    def forward(self, input: Float[Tensor, "*B normalized_shape"]) -> Float[Tensor, "*B normalized_shape"]: ...

class Dropout(Module):
    def __init__(self, p: float = 0.5, inplace: bool = False) -> None: ...
    def forward(self, input: Shaped[Tensor, "*A"]) -> Shaped[Tensor, "*A"]: ...

class ReLU(Module):
    def __init__(self, inplace: bool = False) -> None: ...
    def forward(self, input: Shaped[Tensor, "*A"]) -> Shaped[Tensor, "*A"]: ...
