from jaxtyping import Float
from torch import Tensor


# torch infers -1 only when the other sizes divide the total
# expect-error: can't infer the size -1: b * t may not be divisible by 3
def triples(x: Float[Tensor, "b t"]) -> Float[Tensor, "n 3"]:
    return x.reshape(-1, 3)
