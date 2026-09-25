# Python's math module. A float is a 0-d array; an int argument is a value
# the checker may know, so domain errors are preconditions.

from typing import overload

@overload
def sqrt(x: int) -> float:
    assert x >= 0
@overload
def sqrt(x: float) -> float: ...

@overload
def log(x: int) -> float:
    assert x >= 1
@overload
def log(x: float) -> float: ...
