from jaxtyping import Float
from torch import Tensor


# expect-error: Precondition not provable: prod(A) = 24 = prod(B) = 25
def square(x: Float[Tensor, "4 6"]) -> Float[Tensor, "5 5"]:
    return x.reshape((5, 5))
