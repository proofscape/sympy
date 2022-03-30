"""
Define a minimal set of allowed callables, just sufficient to allow the entire
SymPy unit test suite to pass.
"""

from sympy.assumptions import Q

from sympy.core.basic import Basic
from sympy.core.expr import (
    Add, Expr, Mul, Pow,
)
from sympy.core.function import diff, UndefinedFunction
from sympy.core.mod import Mod
from sympy.core.numbers import Float, Integer, Number, Rational
from sympy.core.relational import Equality, Relational
from sympy.core.singleton import S
from sympy.core.symbol import Symbol, symbols

from sympy.functions.combinatorial.factorials import (
    factorial, factorial2, RisingFactorial,
)
from sympy.functions.elementary.complexes import (
    Abs, arg, conjugate, im, re, sign,
)
from sympy.functions.elementary.exponential import exp, log
from sympy.functions.elementary.hyperbolic import (
    acosh, acoth, acsch, asech, asinh, atanh,
    cosh, coth, csch, sech, sinh, tanh,
)
from sympy.functions.elementary.miscellaneous import (
    cbrt, sqrt, Max, Min,
)
from sympy.functions.elementary.piecewise import (
    Piecewise, ExprCondPair
)
from sympy.functions.elementary.trigonometric import (
    acos, acot, acsc, asec, asin, atan, atan2,
    cos, cot, csc, sec, sin, tan,
)
from sympy.functions.special.bessel import (
    airyai, airyaiprime, airybi, airybiprime, besseli
)
from sympy.functions.special.error_functions import (
    Ci, Ei, Si, li,
)

from sympy.logic.boolalg import Boolean, Or

from sympy.ntheory.factor_ import factorint
from sympy.ntheory.generate import prime, primepi
from sympy.ntheory.primetest import isprime

from sympy.polys.polytools import cancel, factor

from sympy.series.limits import Limit, limit

from sympy.sets.sets import Interval


from typing import Tuple, Dict, Sequence, Union as u

from displaylang.allow import (
    ArgSpec as a,
    AllowedCallable as c,
    StrPermType as s,
    Tail as t
)


Bool = u[Boolean, bool]
Int = u[Integer, int]
iExpr = u[Expr, int]
fExpr = u[Expr, float]
fiExpr = u[Expr, float, int]


basic_callables = [
    c(abs, [Expr]),
    c(pow, [Expr, Expr, t(Int)]),

    c(Add, [t(iExpr)], {'evaluate': bool}),
    c(Mul, [t(iExpr)], {'evaluate': bool}),
    c(Pow, [iExpr, iExpr], {'evaluate': bool}),

    c(UndefinedFunction, [s.UWORD]),

    c(diff, [Expr, t(Symbol)]),

    c(Mod, [iExpr, iExpr]),

    c(Float, [a(fiExpr, s.NUMBER)], {'precision': Int}),
    c(Integer, [a(iExpr, s.NUMBER)]),
    c(Number, [a(fiExpr, s.NUMBER)]),
    c(Rational, [
        [iExpr, iExpr],
        [fExpr],
        [s.NUMBER],
    ]),

    c(Equality, [
        [Expr, Expr],
        [Bool, Bool],
    ]),

    c(S, [a(fiExpr, s.UWORD)], name="S"),
    c(Symbol, [s.UWORD]),
    c(symbols, [s.UWORD_CDL]),

    c(factorial, [Expr]),
    c(factorial2, [Expr]),
    c(RisingFactorial, [Expr, Expr]),

    c(Abs, [Expr], {'evaluate': bool}),
    c(arg, [Expr], {'evaluate': bool}),
    c(conjugate, [Expr], {'evaluate': bool}),
    c(im, [Expr], {'evaluate': bool}),
    c(re, [Expr], {'evaluate': bool}),
    c(sign, [Expr], {'evaluate': bool}),

    c(exp, [Expr], {'evaluate': bool}),
    c(log, [
        [Expr],
        [Expr, Expr],
    ], {'evaluate': bool}),

    c(Piecewise, [t(u[ExprCondPair, Tuple[Expr, Relational]])]),
    c(ExprCondPair, [Expr, Relational]),

    c(cosh, [Expr], {'evaluate': bool}),
    c(coth, [Expr], {'evaluate': bool}),
    c(csch, [Expr], {'evaluate': bool}),
    c(sech, [Expr], {'evaluate': bool}),
    c(sinh, [Expr], {'evaluate': bool}),
    c(tanh, [Expr], {'evaluate': bool}),
    c(acosh, [Expr], {'evaluate': bool}),
    c(acoth, [Expr], {'evaluate': bool}),
    c(acsch, [Expr], {'evaluate': bool}),
    c(asech, [Expr], {'evaluate': bool}),
    c(asinh, [Expr], {'evaluate': bool}),
    c(atanh, [Expr], {'evaluate': bool}),

    c(sqrt, [Expr], {'evaluate': bool}),
    c(cbrt, [Expr], {'evaluate': bool}),

    c(Max, [t(Expr)]),
    c(Min, [t(Expr)]),

    c(cos, [Expr], {'evaluate': bool}),
    c(cot, [Expr], {'evaluate': bool}),
    c(csc, [Expr], {'evaluate': bool}),
    c(sec, [Expr], {'evaluate': bool}),
    c(sin, [Expr], {'evaluate': bool}),
    c(tan, [Expr], {'evaluate': bool}),
    c(acos, [Expr], {'evaluate': bool}),
    c(acot, [Expr], {'evaluate': bool}),
    c(acsc, [Expr], {'evaluate': bool}),
    c(asec, [Expr], {'evaluate': bool}),
    c(asin, [Expr], {'evaluate': bool}),
    c(atan, [Expr], {'evaluate': bool}),
    c(atan2, [Expr, Expr], {'evaluate': bool}),

    c(airyai, [Expr]),
    c(airyaiprime, [Expr]),
    c(airybi, [Expr]),
    c(airybiprime, [Expr]),
    c(besseli, [Expr, Expr]),

    c(Ci, [Expr]),
    c(Ei, [Expr]),
    c(Si, [Expr]),
    c(li, [Expr]),

    c(Or, [t(Bool)], {'evaluate': bool}),

    c(factorint, [Int], {'visual': bool}),
    c(prime, [Int]),
    c(primepi, [Int]),
    c(isprime, [Int]),

    c(cancel, [Expr, t(Symbol)], {'_signsimp': bool}),
    c(factor, [Expr, t(Symbol)], {'deep': bool}),

    c(Limit, [Expr, Symbol, Expr], {'dir': s.SIGN}),
    c(limit, [Expr, Symbol, Expr], {'dir': s.SIGN}),

    c(Interval, [Expr, Expr]),
]

other_callables = [
    c(Q.even, [Expr], name='EvenPredicate'),
    c(Expr.is_polynomial, [t(Symbol)], method_of=Expr),
    c(Basic.subs, [
        [Expr, Expr],
        [u[Dict[Expr, Expr], Sequence[Tuple[Expr, Expr]]]],
    ], {'simultaneous': bool}, method_of=Basic),
    c(Interval.Lopen, [Expr, Expr], classmethod_of=Interval),
    c(Interval.Ropen, [Expr, Expr], classmethod_of=Interval),
    c(Interval.open, [Expr, Expr], classmethod_of=Interval),
]

from sympy import (
    beta, E, Eq, EulerGamma, gamma, GoldenRatio, I, ln,
    oo, pi, Poly, solve, zeta,
)

other_basic_names = {
    'Q': Q,
    'rf': RisingFactorial,
    'max': Max,
    'min': Min,
    'beta': beta,
    'E': E,
    'Eq': Eq,
    'EulerGamma': EulerGamma,
    'gamma': gamma,
    'GoldenRatio': GoldenRatio,
    'I': I,
    'ln': ln,
    'oo': oo,
    'pi': pi,
    'Poly': Poly,
    'solve': solve,
    'zeta': zeta,
}

callables = basic_callables + other_callables
basic_callables_dict = {ac.name: ac.callable for ac in basic_callables}
global_dict = {**basic_callables_dict, **other_basic_names}
