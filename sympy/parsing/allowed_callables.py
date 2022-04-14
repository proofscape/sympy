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
from sympy.core.relational import Equality
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
    c(abs, [fiExpr]),
    c(pow, [fiExpr, fiExpr, t(Int)]),

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

    # To be clear: supporting calls to `S` here means supporting cases where
    # `parse_expr()` has been passed a string "...S(...)..." which itself
    # describes a call to `S`.
    c(S, [a(u[int, float, Integer, Float], s.UWORD)], name="S"),

    c(Symbol, [s.UWORD]),
    c(symbols, [s.UWORD_CDL]),

    c(factorial, [iExpr]),
    c(factorial2, [iExpr]),
    c(RisingFactorial, [iExpr, iExpr]),

    c(Abs, [fiExpr], {'evaluate': bool}),
    c(arg, [fiExpr], {'evaluate': bool}),
    c(conjugate, [fiExpr], {'evaluate': bool}),
    c(im, [fiExpr], {'evaluate': bool}),
    c(re, [fiExpr], {'evaluate': bool}),
    c(sign, [fiExpr], {'evaluate': bool}),

    c(exp, [fiExpr], {'evaluate': bool}),
    c(log, [
        [fiExpr],
        [fiExpr, fiExpr],
    ], {'evaluate': bool}),

    c(cosh, [fiExpr], {'evaluate': bool}),
    c(coth, [fiExpr], {'evaluate': bool}),
    c(csch, [fiExpr], {'evaluate': bool}),
    c(sech, [fiExpr], {'evaluate': bool}),
    c(sinh, [fiExpr], {'evaluate': bool}),
    c(tanh, [fiExpr], {'evaluate': bool}),
    c(acosh, [fiExpr], {'evaluate': bool}),
    c(acoth, [fiExpr], {'evaluate': bool}),
    c(acsch, [fiExpr], {'evaluate': bool}),
    c(asech, [fiExpr], {'evaluate': bool}),
    c(asinh, [fiExpr], {'evaluate': bool}),
    c(atanh, [fiExpr], {'evaluate': bool}),

    c(sqrt, [fiExpr], {'evaluate': bool}),
    c(cbrt, [fiExpr], {'evaluate': bool}),

    c(Max, [t(fiExpr)]),
    c(Min, [t(fiExpr)]),

    c(cos, [fiExpr], {'evaluate': bool}),
    c(cot, [fiExpr], {'evaluate': bool}),
    c(csc, [fiExpr], {'evaluate': bool}),
    c(sec, [fiExpr], {'evaluate': bool}),
    c(sin, [fiExpr], {'evaluate': bool}),
    c(tan, [fiExpr], {'evaluate': bool}),
    c(acos, [fiExpr], {'evaluate': bool}),
    c(acot, [fiExpr], {'evaluate': bool}),
    c(acsc, [fiExpr], {'evaluate': bool}),
    c(asec, [fiExpr], {'evaluate': bool}),
    c(asin, [fiExpr], {'evaluate': bool}),
    c(atan, [fiExpr], {'evaluate': bool}),
    c(atan2, [fiExpr, fiExpr], {'evaluate': bool}),

    c(airyai, [fiExpr]),
    c(airyaiprime, [fiExpr]),
    c(airybi, [fiExpr]),
    c(airybiprime, [fiExpr]),
    c(besseli, [fiExpr, fiExpr]),

    c(Ci, [fiExpr]),
    c(Ei, [fiExpr]),
    c(Si, [fiExpr]),
    c(li, [fiExpr]),

    c(Or, [t(Bool)], {'evaluate': bool}),

    c(factorint, [Int], {'visual': bool}),
    c(prime, [Int]),
    c(primepi, [Int]),
    c(isprime, [Int]),

    c(cancel, [Expr, t(Symbol)], {'_signsimp': bool}),
    c(factor, [Expr, t(Symbol)], {'deep': bool}),

    c(Limit, [Expr, Symbol, fiExpr], {'dir': s.SIGN}),
    c(limit, [Expr, Symbol, fiExpr], {'dir': s.SIGN}),

    c(Interval, [fiExpr, fiExpr]),
]

other_callables = [
    c(Q.even, [iExpr], name='EvenPredicate'),
    c(Expr.is_polynomial, [t(Symbol)], method_of=Expr),
    c(Basic.subs, [
        [Expr, Expr],
        [u[Dict[Expr, Expr], Sequence[Tuple[Expr, Expr]]]],
    ], {'simultaneous': bool}, method_of=Basic),
    c(Interval.Lopen, [fiExpr, fiExpr], classmethod_of=Interval),
    c(Interval.Ropen, [fiExpr, fiExpr], classmethod_of=Interval),
    c(Interval.open, [fiExpr, fiExpr], classmethod_of=Interval),
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
