from jaxtyping import Float
from torch import Tensor


# a pair has positions 0 and 1 (or -2 and -1)
# expect-error: in second, `return pair[:, 2]`
# expect-error: index 2 may be out of range for a dim of size 2
def second(pair: Float[Tensor, "b 2 d"]) -> Float[Tensor, "b d"]:
    return pair[:, 2]
