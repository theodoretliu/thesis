from demo import *
from pysmt.shortcuts import Symbol, Equals
from pysmt.solvers.solver import Solver


def test_prove():
    x = gen_sym()

    solver = Solver()
    y = x

    assert prove(solver, Equals(y, x))
