"""Allowed callables, for safer parsing. """

import re

from typeguard import check_type


class ParsingExcep(Exception):
    ...


class BadArgs(ParsingExcep):
    ...


CNAME_PATTERN = re.compile(r'[_a-zA-Z]\w*$')
UNICODE_WORD = re.compile(r'\w+$')
NUMBER_PATTERN = re.compile(r'\d*(\.\d*)?(e\d+)?')


class StrPermType:
    # Reject all strings:
    NONE = "NONE"
    # Any string is accepted:
    ANY = "ANY"
    # Accept only CNAMEs, i.e. `[_a-zA-Z]\w*`:
    CNAME = "CNAME"
    # Accept a comma-delimited list (ignoring whitespace) of CNAMEs:
    CNAME_CDL = "CNAME_CDL"
    # Accept any Unicode word:
    UWORD = "UWORD"
    # Accept a comma-delimited list (ignoring whitespace) of UWORDs:
    UWORD_CDL = "UWORD_CDL"
    # Any numeric literal (int or float)
    NUMBER = "NUMBER"


def string_is_permitted(s, perm_type):
    """
    Check whether a string is acceptable under a given string permission type.

    @param s: str
        The string to be checked.
    @param perm_type:  StrPermType
        The permission type that the string is expected to satisfy.
    @return: boolean, True iff the string is acceptable.
    """
    if perm_type == StrPermType.NONE:
        return False
    if perm_type == StrPermType.ANY:
        return True
    if perm_type == StrPermType.CNAME:
        return CNAME_PATTERN.match(s)
    if perm_type == StrPermType.CNAME_CDL:
        parts = [p.strip() for p in s.split(',')]
        return all(CNAME_PATTERN.match(p) for p in parts)
    if perm_type == StrPermType.UWORD:
        return UNICODE_WORD.match(s)
    if perm_type == StrPermType.UWORD_CDL:
        parts = [p.strip() for p in s.split(',')]
        return all(UNICODE_WORD.match(p) for p in parts)
    if perm_type == StrPermType.NUMBER:
        return s and NUMBER_PATTERN.match(s)
    return False


class ArgSpec:
    """
    Define the allowed types for a single argument.
    """

    def __init__(self, t=None, s=None):
        """
        @param t: a type that the argument is allowed to be.

            A type can be any built-in type (except `str`, see below),
            or any user-defined class,
            or any of the type hint constructions listed
            [here](https://typeguard.readthedocs.io/en/latest/userguide.html#supported-typing-types)
            from the `typing` module.

            You cannot allow strings by listing `str` here. If you want to
            allow strings you must use the `s` kwarg.

        @param s: StrPermType or None. If a StrPermType, then the
            argument is allowed to be a string iff it matches this
            type.
        """
        self.typehint = t
        self.strings = s

    def __str__(self):
        p = []
        if self.typehint is not None:
            p.append(str(self.typehint))
        if self.strings is not None:
            p.append(f'str: {self.strings}')
        return ' or '.join(p)

    @classmethod
    def convert(cls, other):
        """
        Convert something other than an `ArgSpec` into an `ArgSpec`.
        """
        if isinstance(other, (ArgSpec, Tail)):
            return other
        elif isinstance(other, str):
            # FIXME: we should turn StrPermType into a proper Enum class,
            #  and then use a better check here.
            return cls(s=other)
        else:
            return cls(other)

    def ok(self, arg):
        """
        Test whether a given arg matches the spec.
        """
        if isinstance(arg, str):
            if isinstance(self.strings, str):
                # FIXME: use better test based on proper Enum class
                return string_is_permitted(arg, self.strings)
            return False
        else:
            try:
                check_type('arg', arg, self.typehint)
            except TypeError:
                return False
            else:
                return True


class Tail:
    """
    Provide an arg spec for the tail end of a free list `*args` of positional args.
    """

    def __init__(self, spec):
        self.spec = ArgSpec.convert(spec)

    def ok(self, args):
        return all(self.spec.ok(a) for a in args)

    def first_failing_index(self, args):
        for i, a in enumerate(args):
            if not self.spec.ok(a):
                return i
        return -1


class AllowedCallable:

    def __init__(self, callable, *pos_arg_specs, **kwarg_specs):
        """
        @param callable: the callable itself, which we represent.
        @param pos_arg_specs: one arg spec for each positional arg the callable
            accepts. Each spec can be an actual `ArgSpec` instance, or anything
            convertible thereto. The last spec can be a `Tail` instance.
        @param kwarg_specs: up to one arg spec for each keyword arg the callable
            accepts. Omitting the spec for a keyword arg means we have not
            allowed anything to be passed for that arg. Each spec can be an
            actual `ArgSpec` instance, or anything convertible thereto.
        """
        self.callable = callable

        pos_arg_specs = [ArgSpec.convert(s) for s in pos_arg_specs]
        self.req_arg_specs = []
        self.tail = None
        if pos_arg_specs:
            if isinstance(pos_arg_specs[-1], Tail):
                self.req_arg_specs = pos_arg_specs[:-1]
                self.tail = pos_arg_specs[-1]
            else:
                self.req_arg_specs = pos_arg_specs
        self.num_req_args = len(self.req_arg_specs)

        self.kwarg_specs = {k: ArgSpec.convert(v) for k, v in kwarg_specs.items()}

    @property
    def name(self):
        name = getattr(self.callable, '__name__', None)
        if not name:
            raise ParsingExcep(f'Callable {self.callable} has no name.')
        return name

    @property
    def full_name(self):
        module = getattr(self.callable, '__module__', None)
        if not module:
            raise ParsingExcep(f'Callable {self.callable} has no module.')
        return f'{module}.{self.name}'

    def write_permission_err_msg(self, arg_spec, pos=None, kw=None):
        if pos is not None:
            msg = f'For positional arg {pos}'
        else:
            msg = f'For keyword arg "{kw}"'
        msg += f' of the `{self.full_name}` function, arg spec is: {arg_spec}.'
        return msg

    def __call__(self, *args, **kwargs):
        """
        Call our callable and return the result, but only if the args and kwargs
        pass all checks. Raise `BadArgs` if any check fails.
        """
        if self.check_args(args, kwargs):
            return self.callable(*args, **kwargs)

    def check_args(self, A, K):
        """
        Check the given arguments for any violation of the type permissions
        for this callable. If there is any violotion, a ParsingExcep (possibly
        BadArgs) is raised.

        @param A: list of positional args
        @param K: dict of keyword args
        @return: True iff pass all checks.
        """
        N = len(A)
        n = self.num_req_args

        if N < n:
            raise BadArgs(f'Missing positional args to function `{self.full_name}`.')

        A_tail = None
        if N > n:
            if self.tail is None:
                raise BadArgs(f'Too many positional args to function `{self.full_name}`.')
            A, A_tail = A[:n], A[n:]

        for i, (a, spec) in enumerate(zip(A, self.req_arg_specs)):
            if not spec.ok(a):
                raise BadArgs(self.write_permission_err_msg(spec, pos=i))

        if A_tail:
            i = self.tail.first_failing_index(A_tail)
            if i >= 0:
                raise BadArgs(self.write_permission_err_msg(self.tail.spec, pos=i))

        for k, v in K.items():
            spec = self.kwarg_specs.get(k)
            if spec is None:
                raise ParsingExcep(f'No arg spec for keyword arg `{k}` to function `{self.full_name}`.')
            if not spec.ok(v):
                raise BadArgs(self.write_permission_err_msg(spec, kw=k))
        return True


class NamedAllowedCallable(AllowedCallable):
    """
    An AllowedCallable where we need to override the name.

    Example: For the SymPy `Matrix` class, `Matrix.__name__` returns
    the name "MutableDenseMatrix", instead of the name "Matrix" that we want.
    """

    def __init__(self, callable, name, *pos_arg_specs, **kwarg_specs):
        super().__init__(callable, *pos_arg_specs, **kwarg_specs)
        self._name = name

    @property
    def name(self):
        return self._name


from typing import List, Optional as o, Union as u

from sympy.assumptions import Q

from sympy.core.expr import (
    Add, Expr, Mul, Pow,
)
from sympy.core.numbers import Float, Integer, Number, Rational
from sympy.core.relational import Equality
from sympy.core.symbol import Symbol

from sympy.functions.combinatorial.factorials import factorial, factorial2
from sympy.functions.elementary.complexes import (
    Abs, arg, conjugate, im, re, sign,
)
from sympy.functions.elementary.exponential import exp, log
from sympy.functions.elementary.hyperbolic import (
    acosh, acoth, acsch, asech, asinh, atanh,
    cosh, coth, csch, sech, sinh, tanh,
)
from sympy.functions.elementary.miscellaneous import sqrt, cbrt
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

from sympy.ntheory.factor_ import factorint


c = AllowedCallable
nc = NamedAllowedCallable
s = StrPermType
a = ArgSpec
t = Tail

Int = u[Integer, int]
iExpr = u[Expr, int]
fiExpr = u[Expr, float, int]

# This is meant to be a minimal list of allowed callables, just sufficient to
# allow the entire suite of SymPy unit tests to pass.
sympy_unit_tests_callables = [
    c(abs, Expr),
    c(pow, Expr, Expr, t(Int)),

    c(Q.even, Expr),

    c(Add, t(Expr), evaluate=bool),
    c(Mul, t(Expr), evaluate=bool),
    c(Pow, iExpr, iExpr, evaluate=bool),

    c(Float, a(fiExpr, s.NUMBER)),
    c(Integer, a(iExpr, s.NUMBER)),
    c(Number, a(fiExpr, s.NUMBER)),
    c(Rational, a(fiExpr, s.NUMBER), t(iExpr)),

    c(Equality, Expr, Expr),
    c(Symbol, s.UWORD),

    c(factorial, Expr),
    c(factorial2, Expr),

    c(Abs, Expr, evaluate=bool),
    c(arg, Expr, evaluate=bool),
    c(conjugate, Expr, evaluate=bool),
    c(im, Expr, evaluate=bool),
    c(re, Expr, evaluate=bool),
    c(sign, Expr, evaluate=bool),

    c(exp, Expr, evaluate=bool),
    c(log, Expr, evaluate=bool),

    c(cosh, Expr, evaluate=bool),
    c(coth, Expr, evaluate=bool),
    c(csch, Expr, evaluate=bool),
    c(sech, Expr, evaluate=bool),
    c(sinh, Expr, evaluate=bool),
    c(tanh, Expr, evaluate=bool),
    c(acosh, Expr, evaluate=bool),
    c(acoth, Expr, evaluate=bool),
    c(acsch, Expr, evaluate=bool),
    c(asech, Expr, evaluate=bool),
    c(asinh, Expr, evaluate=bool),
    c(atanh, Expr, evaluate=bool),

    c(sqrt, Expr, evaluate=bool),
    c(cbrt, Expr, evaluate=bool),

    c(cos, Expr, evaluate=bool),
    c(cot, Expr, evaluate=bool),
    c(csc, Expr, evaluate=bool),
    c(sec, Expr, evaluate=bool),
    c(sin, Expr, evaluate=bool),
    c(tan, Expr, evaluate=bool),
    c(acos, Expr, evaluate=bool),
    c(acot, Expr, evaluate=bool),
    c(acsc, Expr, evaluate=bool),
    c(asec, Expr, evaluate=bool),
    c(asin, Expr, evaluate=bool),
    c(atan, Expr, evaluate=bool),
    c(atan2, Expr, Expr, evaluate=bool),

    c(airyai, Expr),
    c(airyaiprime, Expr),
    c(airybi, Expr),
    c(airybiprime, Expr),
    c(besseli, Expr, Expr),

    c(Ci, Expr),
    c(Ei, Expr),
    c(Si, Expr),
    c(li, Expr),

    c(factorint, Int, visual=bool),
]

sympy_unit_tests_c2ac = {ac.callable: ac for ac in sympy_unit_tests_callables}
