"""Allowed callables, for safer parsing. """

import re

from typeguard import check_type


class ParsingExcep(Exception):
    ...


class BadArgs(ParsingExcep):
    """
    Represents a failure of given positional and keyword args to match an
    expected (alternative) signature for an AllowedCallable.
    """
    ...


class CannotCall(ParsingExcep):
    """
    Represents a failure to call an AllowedCallable `ac`.
    Stores a list of `BadArgs` exceptions, one for each alternative signature
    of `ac`.
    """

    def __init__(self, badArgsExceps):
        self.badArgsExceps = badArgsExceps


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
    # + or -
    SIGN = "SIGN"


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
    if perm_type == StrPermType.SIGN:
        return s in ["+", "-"]
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


class CheckedArgs:
    """
    Represents the result of checking a given pair (A0, K0) of positional and
    keyword args against a Signature.

    An instance of this class will contain a pair (A1, K1)
    of args that are okay to be passed on to the callable in question. Only
    these args, not the given ones, must be passed onward. This is because
    the given (A0, K0) may contain keyword args passed positionally, and we
    must be sure that these go to the right arg. There could be a difference
    because `AllowedCallable`s are sometimes a work-in-progress, in which not
    every keyword arg yet has a typedef.

    Example:

    Consider a function,

        def foo(bar, spam=None, baz=None):
            ...

    and suppose the AllowedCallable that has been defined is a work in progress,

        ac = AllowedCallable(foo, [int], {baz=StrPermType.ANY})

    in which the developer has not yet made the arg spec for the `spam` kwarg.
    Note: In such a case, the developer _should_ have set `kwargs_complete=False`
    (see `AllowedCallable.__init__()`), to disallow positionally passed kwargs.

    When we then check the function call

        foo(1, 'lorem.ipsum[0]')

    against `ac`, we will approve the string 'lorem.ipsum[0]' for use with
    kwarg `baz`. However, if we just forwarded the args to `foo` as they are,
    then kwarg `spam` would receive this string. This could be a security
    vulnerability.
    """

    def __init__(self, signature, checked_pos_args, checked_kwargs):
        self.signature = signature
        self.A = checked_pos_args
        self.K = checked_kwargs


class Signature:
    """
    Represents a single accepted function signature, including arg specs for
    both positional and keyword arguments.
    """

    def __init__(self, allowed_callable, pos_arg_specs, kwarg_specs, kwargs_complete):
        self.ac = allowed_callable
        self.kwargs_complete = kwargs_complete

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
        self.num_kwargs = len(self.kwarg_specs)

    def write_arg_err_msg(self, arg_spec, pos=None, kw=None):
        if pos is not None:
            msg = f'For positional arg {pos}'
        else:
            msg = f'For keyword arg "{kw}"'
        msg += f' of the `{self.ac.full_name}` function, arg spec is: {arg_spec}.'
        return msg

    def check_args(self, A, K):
        """
        Check the given arguments for any violation of the type permissions
        for this signature. If there is any violotion, a BadArgs is raised.

        @param A: list of positional args
        @param K: dict of keyword args
        @raises: BadArgs if any arguments violate type permissions
        @return: CheckedArgs instance, in which there are no ambiguities,
            namely, keyword args are passed only as such, never positionally.
        """
        N = len(A)
        n = self.num_req_args
        allow_positional_kwargs = self.kwargs_complete

        if N < n:
            raise BadArgs(f'Missing positional args to function `{self.ac.full_name}`.')

        A0, A_tail = A[:n], A[n:]

        for i, (a, spec) in enumerate(zip(A0, self.req_arg_specs)):
            if not spec.ok(a):
                raise BadArgs(self.write_arg_err_msg(spec, pos=i))

        A1, K1 = A, K

        if A_tail:
            if self.tail:
                i = self.tail.first_failing_index(A_tail)
                if i >= 0:
                    raise BadArgs(self.write_arg_err_msg(self.tail.spec, pos=i))
            elif allow_positional_kwargs:
                if len(A_tail) > self.num_kwargs:
                    raise BadArgs(f'Too many positional args to function `{self.ac.full_name}`.')
                A1 = A0
                K1 = {k: a for k, a in zip(self.kwarg_specs.keys(), A_tail)}
                for k, v in K.items():
                    if k in K1:
                        raise BadArgs(f'{self.ac.full_name} got multiple values for argument {k}')
                    else:
                        K1[k] = v
            else:
                raise BadArgs(f'Positional kwargs are not allowed for {self.ac.full_name}')

        for k, v in K1.items():
            spec = self.kwarg_specs.get(k)
            if spec is None:
                raise BadArgs(f'No arg spec for keyword arg `{k}` to function `{self.ac.full_name}`.')
            if not spec.ok(v):
                raise BadArgs(self.write_arg_err_msg(spec, kw=k))

        return CheckedArgs(self, A1, K1)


class AllowedCallable:

    def __init__(self, callable, pos_arg_specs, kwarg_specs=None,
                 name=None, self_type=None, kwargs_complete=True):
        """
        @param callable: the callable itself, which we represent.

        @param pos_arg_specs: In the "basic" case, this is a list of arg specs,
            one for each positional arg the callable accepts. Each spec can be
            an actual `ArgSpec` instance, or anything convertible thereto (by
            the `ArgSpec.convert()` class method). The last spec can be a
            `Tail` instance.

            In the "advanced" case, this can be a list of such lists, as a way
            of specifying alternative function signatures. For example, maybe
            the function either accepts two ints, or one float.

        @param kwarg_specs: In the "basic" case, this is a dictionary of arg
            specs, one for each keyword arg the callable accepts. Omitting the
            spec for a keyword arg means we have not allowed anything to be
            passed for that arg. Each spec can be an actual `ArgSpec` instance,
            or anything convertible thereto. In the "advanced" case, this can
            be a list of such dictionaries.

            If `pos_arg_specs` is a list of lists, but `kwarg_specs` is a single
            dictionary, then these are the kwarg specs for every alternative
            list of positional arg specs. If `kwarg_specs` is also a list then
            dictionaries after the first inherit from the previous one and
            override only those entries they define. If the list of dicts is
            shorter than `pos_arg_specs`, empty dicts are inserted at the end
            to make it the same length.

        @param name: If `None`, the name of the callable will be automatically
            determined from its `__qualname__` attribute. If a string, this
            name will be used instead. Example: For the SymPy `Matrix` class,
            `Matrix.__qualname__` returns the name "MutableDenseMatrix", instead
            of the name "Matrix" that we probably want.

            If `callable.__qualname__` is undefined (example: the SymPy `S`
            object), then you must supply a name here.

            The intention is that this name be usable as the name of this callable
            when parsing Python code where this callable should be available.

        @param self_type: When the callable is a class method, you must provide
            the arg spec for the first (i.e. `self`) argument here, and not in
            `pos_arg_specs`. May be an actual `ArgSpec` instance, or anything
            convertible thereto. May be a list of alternatives of equal length
            to `pos_arg_specs` when that is a list of lists.

        @param kwargs_complete: If False, we will not allow kwargs to be passed
            positionally. This is meant to support gradual development, so that
            a developer can define an AllowedCallable in which some, but not yet
            all, of the kwargs have type definitions. If True (the default), then
            kwargs passed positionally will be mapped to kwargs in the order
            defined in `kwarg_specs`.

            Note: If the kwarg specs are incomplete, but the developer forgets
            to set `kwargs_complete` to False, it will lead to unexpected
            behavior, but not dangerous behavior, thanks to the use of
            `CheckedArgs`. See docstring for that class for more.
        """
        self.callable = callable
        self.kwargs_complete = kwargs_complete

        qualname = getattr(callable, '__qualname__', None)
        self_type_given = ((isinstance(self_type, list) and len(self_type) > 0) or self_type is not None)

        if qualname is None:
            if not name:
                raise ValueError('Callable has no __qualname__. Must supply a name.')
        else:
            dotted = (qualname.find('.') >= 0)
            if dotted and not self_type_given:
                raise ValueError(
                    'Callable does not appear to be a top-level function or class.'
                    ' For class methods, you must supply a `self_type` arg spec.'
                )
            if self_type_given and not dotted:
                raise ValueError(
                    'Callable appears to be a top-level function or class.'
                    ' You should not supply a `self_type` arg spec.'
                )

        self._name = name or qualname

        P, K = pos_arg_specs, (kwarg_specs or {})
        if not isinstance(P, list):
            raise ValueError('pos_arg_specs must be a list')
        if len(P) == 0 or not isinstance(P[0], list):
            P = [P]

        if self_type_given:
            S = [self_type] if not isinstance(self_type, list) else self_type
            d = len(P) - len(S)
            while d > 0:
                S.append(S[-1])
                d -= 1
            P1 = []
            for s, p in zip(S, P):
                P1.append([s] + p)
            P = P1

        if isinstance(K, dict):
            K = [K]
        d = len(P) - len(K)
        while d > 0:
            K.append({})
            d -= 1

        self.alternatives = []
        prev_k = {}
        for p, k0 in zip(P, K):
            k1 = {**prev_k, **k0}
            self.alternatives.append(Signature(self, p, k1, kwargs_complete))
            prev_k = k1

    @property
    def name(self):
        return self._name

    @property
    def full_name(self):
        module = getattr(self.callable, '__module__', '<no_module>')
        return f'{module}.{self.name}'

    def __call__(self, *args, **kwargs):
        """
        Call our callable and return the result, but only if the args and kwargs
        satisfied one of our alternative signatures. Raise `CannotCall` if no
        signature was satisfied.
        """
        result = self.check_args(args, kwargs)
        if isinstance(result, CheckedArgs):
            A1, K1 = result.A, result.K
            return self.callable(*A1, **K1)
        raise CannotCall(result)

    def check_args(self, A, K):
        """
        Check the given arguments to see if they satisfy any of the alternative
        signautres allowed for this callable.

        @param A: list of positional args
        @param K: dict of keyword args
        @return: the CheckedArgs for the first Signature that was satisfied, or
            else a list of the BadArgs exceptions raised by each of the Signatures.
        """
        badArgExceps = []
        for sig in self.alternatives:
            try:
                checked_args = sig.check_args(A, K)
            except BadArgs as ba:
                badArgExceps.append(ba)
            else:
                return checked_args
        return badArgExceps


from typing import List, Tuple, Dict, Sequence, Optional as o, Union as u

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


c = AllowedCallable
s = StrPermType
a = ArgSpec
t = Tail

Bool = u[Boolean, bool]
Int = u[Integer, int]
iExpr = u[Expr, int]
fExpr = u[Expr, float]
fiExpr = u[Expr, float, int]

# This is meant to be a minimal list of allowed callables, just sufficient to
# allow the entire suite of SymPy unit tests to pass.
sympy_unit_tests_callables = [
    c(abs, [Expr]),
    c(pow, [Expr, Expr, t(Int)]),

    c(Q.even, [Expr], name='EvenPredicate'),

    c(Add, [t(iExpr)], {'evaluate': bool}),
    c(Mul, [t(iExpr)], {'evaluate': bool}),
    c(Pow, [iExpr, iExpr], {'evaluate': bool}),

    c(Expr.is_polynomial, [t(Symbol)], self_type=Expr),
    c(Basic.subs, [
        [Expr, Expr],
        [u[Dict[Expr, Expr], Sequence[Tuple[Expr, Expr]]]],
    ], {'simultaneous': bool}, self_type=Basic),

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
    c(S, [fiExpr], name="S"),
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
    c(Interval.Lopen, [Expr, Expr], self_type=Interval),
    c(Interval.Ropen, [Expr, Expr], self_type=Interval),
    c(Interval.open, [Expr, Expr], self_type=Interval),
]

sympy_unit_tests_c2ac = {ac.callable: ac for ac in sympy_unit_tests_callables}
