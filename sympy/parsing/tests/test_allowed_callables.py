"""
At last two tests for each allowed callable: one acceptance, and one rejection.
"""

from displaylang.exceptions import CannotCall
from displaylang.evaluate import evaluate_expression

from sympy.abc import x, y, z, n, m, p, a, b, c
from sympy.parsing.sympy_parser import make_expression_evaluator
from sympy.testing.pytest import raises

# One fixed evaluator for all tests:
expression_evaluator = make_expression_evaluator()
local_vars = {
    'x': x, 'y': y, 'z': z, 'n': n, 'm': m, 'p': p,
    'a': a, 'b': b, 'c': c,
}

def test_allow_call():
    """
    Test that calls with acceptable arguments go through without raising any
    exception.

    NOTE: In `parse_expr()`, the code passed to an `ExpressionEvaluator`
    has already been passed through the transformers. In particular, unknown
    names have been wrapped in calls to `Symbol()`.

    Here, this is not the case. We are passing a code string directly to the
    evaluator. So, in order to test cases where functions receive SymPy
    `Expr` instances as arguments, we use a `local_dict` that defines a few
    variables.
    """
    for code in [
        'abs(-3)',
        'abs(-3*x)',
        'pow(2, 3)',
        'pow(x + 2, 3)',
        'Add(2, 3)',
        'Add(2, x, y + z)',
        'Mul(2, 3)',
        'Mul(2, x, y + z)',
        'Pow(2, 3)',
        'Pow(2, y + z)',
        'UndefinedFunction("foo")',
        'diff(x**2)',
        'diff(x**2*y, x)',
        'Mod(7, 3)',
        'Mod(7, m)',
        'Mod(a + b, m)',
        'Float(3.14)',
        'Float(3)',
        'Float(S(22)/7)',
        'Float(S(22)/7, precision=5)',
        'Float("3")',
        'Integer(3)',
        'Integer(S(3))',
        'Integer("3")',
        'Number(3.14)',
        'Number(3)',
        'Number(S(22)/7)',
        'Rational(22, 7)',
        'Rational(3.14)',
        'Rational("3")',
        'Equality(x + y, y + z)',
        'Equality(x < y, y < z)',
        'S(3)',
        'S(3.14)',
        'S("alpha")',
        'S("alpha12")',
        'Symbol("alpha")',
        'Symbol("alpha12")',
        'symbols("f, g, h")',
        'symbols("f0, g1, h2")',
        'factorial(5)',
        'factorial(5 + n)',
        'factorial2(5)',
        'factorial2(5 + n)',
        'RisingFactorial(x**2 + 3, 5)',
        'log(3, 2)',
        'log(3, 2.5)',
        'log(3.5, 2.5)',
        'log(3.5, x + 2)',
        'Max(1, 2.2, x + 3)',
        'Min(1, 2.2, x + 3)',
        'besseli(n, z)',
        'Or(x < y, y < z)',
        'factorint(210)',
        'factorint(S(210))',
        'prime(4)',
        'prime(S(4))',
        'prime(100)',
        'prime(S(100))',
        'isprime(101)',
        'isprime(S(101))',
        'cancel((2*x**2 - 2)/(x**2 - 2*x + 1))',
        'cancel((2*x**2 - 2)/(x**2 - 2*x + 1), x)',
        'factor(2*x**2 - 2, x)',
        'Limit((3*x**2 + 1)/(2*x**2 + 5), x, oo)',
        'Limit((3*x**2 + 1)/(2*x**2 + 5), x, b)',
        'Limit(sin(x)/x, x, 0, dir="+")',
        'limit((3*x**2 + 1)/(2*x**2 + 5), x, oo)',
        'limit(sin(x)/x, x, 0, dir="+")',
        'Interval(a, b)',
        'Interval(-2, 2)',
        'Q.even(2)',
        'Q.even(x + 3)',
        'x.is_polynomial()',
        'x.is_polynomial(y)',
        '(x + y).is_polynomial()',
        '(x + y).subs(x, a)',
        '(x + y).subs({x: a})',
        '(x + y).subs([(x, a), (y, b)])',
        '(x + y).subs({x: y, y: x}, simultaneous=True)',
        'Interval.Lopen(a, 3.14)',
        'Interval.Ropen(a, 3.14)',
        'Interval.open(a, 3.14)',
    ]:
        expression_evaluator.set_local_vars(local_vars)
        evaluate_expression(expression_evaluator, code)


def test_disallow_call():
    """
    Test that calls with unacceptable arguments are disallowed.
    """
    for code in [
        'abs("foo")',
        'pow(2, "foo")',
        'Add(2, "foo")',
        'Mul(2, "foo")',
        'Pow(2, "foo")',
        'UndefinedFunction("foo.bar")',
        'UndefinedFunction("foo[bar]")',
        'UndefinedFunction("foo(bar)")',
        'diff(x**2*y, 3)',
        'Mod(7, "foo")',
        'Float("foo")',
        'Integer(3.14)',
        'Integer("foo")',
        'Number("foo")',
        'Rational(22.5, 7)',
        'Rational("foo")',
        'Equality(x + y, y < z)',
        'S("foo.bar")',
        'S("foo[bar]")',
        'S("foo(bar)")',
        'Symbol("foo.bar")',
        'Symbol("foo[bar]")',
        'Symbol("foo(bar)")',
        'symbols("f.f, g.g, h.h")',
        'symbols("f(f), g(g), h(h)")',
        'factorial(5.1)',
        'factorial2(5.1)',
        'RisingFactorial(x**2 + 3, "5")',
        'log(3, "foo")',
        'Max(1, 2.2, x + 3, "foo")',
        'Min(1, 2.2, x + 3, "foo")',
        'besseli(n, "foo")',
        'Or(x < y, y + z)',
        'factorint(8.1)',
        'prime(8.1)',
        'primepi(8.1)',
        'isprime(8.1)',
        'cancel("foo")',
        'factor("foo")',
        'Limit(sin(x)/x, 5, 0, dir="+")',
        'limit(sin(x)/x, 5, 0, dir="+")',
        'Interval(2, "foo")',
        'Q.even(2.3)',
        'x.is_polynomial("foo")',
        '(x + y).subs([{x: a}, {y: b}])',
        'Interval.Lopen(a, "foo")',
        'Interval.Ropen(a, "foo")',
        'Interval.open(a, "foo")',
    ]:
        expression_evaluator.set_local_vars(local_vars)
        raises(CannotCall, lambda: evaluate_expression(expression_evaluator, code))


def test_func_of_one_fiExpr():
    for func_name in """
    Abs arg conjugate im re sign exp log
    sqrt cbrt
    cos cot csc sec sin tan
    acos acot acsc asec asin atan
    cosh coth csch sech sinh tanh
    acosh acoth acsch asech asinh atanh
    airyai airyaiprime airybi airybiprime
    Ci Ei Si li
    """.split():
        # Allow:
        for arg in """
        3 3.14 1-2.71828*I x+3
        """.split():
            code = f'{func_name}({arg})'
            expression_evaluator.set_local_vars(local_vars)
            evaluate_expression(expression_evaluator, code)
        # Disallow:
        code = f'{func_name}("foo")'
        expression_evaluator.set_local_vars(local_vars)
        raises(CannotCall, lambda: evaluate_expression(expression_evaluator, code))
