from pysmt.shortcuts import Symbol, is_sat, And, Not, Equals, Max, GT
from pysmt.solvers.solver import Solver
from pysmt.typing import INT
from itertools import zip_longest

i = 0


def gen_sym():
    global i

    i += 1
    return Symbol(f"newvar{i}", INT)


def prove(solver, thm):
    solver.push()

    solver.add_assertion(Not(thm))

    provable = solver.is_sat()

    solver.pop()

    return provable


def broadcast(a, b, solver):
    for x, y in zip(reversed(a), reversed(b)):
        if not is_sat(Equals(a, b)):
            return None

    output = []

    for x, y in zip_longest(reversed(a), reversed(b)):
        typ = gen_sym()
        if x is not None and y is not None:
            solver.add_assertion(Equals(typ, Max(x, y)))
        elif x is not None:
            solver.add_assertion(Equals(typ, x))
        else:
            solver.add_assertion(Equals(typ, y))

        output.append(typ)

    return output


def dot(a, b, solver):
    if len(a) == 1 and len(b) == 1:
        if not prove(solver, Equals(a[0], b[0])):
            return None

        return []

    if len(a) == 2 and len(b) == 2:
        if not prove(solver, Equals(a[1], b[0])):
            return None

        x = gen_sym()
        y = gen_sym()

        solver.add_assertion(Equals(x, a[0]))
        solver.add_assertion(Equals(y, b[1]))

        return [x, y]

    if not a or not b:
        if not b:
            return a

        return b

    if len(b) == 1:
        if not prove(solver, Equals(a[-1], b[-1])):
            return None

        return a[:-1]

    if not prove(solver, Equals(a[-1], b[-2])):
        return None

    return a[:-1] + b[:-2] + [b[-1]]


def npsum(A, axis, solver):
    if axis is None:
        return []

    if isinstance(axis, int):
        if len(A) < axis:
            return None

        return A[:axis] + A[axis + 1 :]

    for ax in axis:
        if len(A) < ax:
            return None

    return [ax for i, ax in enumerate(A) if i not in axis]


def concatenate(As, axis, solver):
    if len(As) == 0:
        return None

    if not all(len(As[0]) == len(b) for b in As):
        return None

    for i, ax in enumerate(zip(*As)):
        if i == axis:
            continue

        if not all(prove(solver, Equals(a, ax[0])) for a in ax):
            return None

    output = []

    for i, ax in enumerate(As[0]):
        typ = gen_sym()
        if i == axis:
            solver.add_assertion(Equals(typ, sum(ax)))
        else:
            solver.add_assertion(Equals(typ, ax[0]))

        output.append(typ)

    return output
