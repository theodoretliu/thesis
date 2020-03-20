Ty1 = Nparray[int, ...]
Ty2 = Nparray[int, ...]

A = TypeVar()

B = IntVar()


@overload
def dot(a: Nparray[A, [B]], b: Nparray[A, [B]]) -> A:
    ...


@overload
def dot(a: Nparray[A, [B, C]], b: Nparray[A, [C, D]]) -> Nparray[A, [C, D]]:
    ...


@overload
def dot(a: Nparray[A, []], b: Nparray[A, [*E]]) -> Nparray[A, [*E]]:
    ...


@overload
def dot(a: Nparray[A, [*E]], b: Nparray[A, []]) -> Nparray[A, [*E]]:
    ...


@overload
def dot(a: Nparray[A, [*E, F]], b: Nparray[A, [F]]) -> Nparray[A, [*E]]:
    ...


@overload
def dot(a: Nparray[A, [*E, F]], b: Nparray[A, [*G, F, H]]) -> Nparray[A, [*E, *G, H]]:
    ...


@overload
def
