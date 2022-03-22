from sympy.parsing.allow import AllowedCallable, Signature, Tail
from sympy.parsing.exceptions import BadArgs, CannotCall
from sympy.testing.pytest import raises


def foo(a, b, c=3, d=4):
    return sum([a, 2*b, 3*c, 4*d])


def bar(a, b, *c, d=4):
    return sum([a, 2*b, 3*sum(c), 4*d])


def test_check_args():
    # kwargs complete:
    ac_foo_1 = AllowedCallable(foo, [int, int], {'c': int, 'd': int})
    # kwargs incomplete, and marked as such:
    ac_foo_2 = AllowedCallable(foo, [int, int], {'d': int}, kwargs_complete=False)
    # kwargs incomplete, forgot to mark as such:
    ac_foo_3 = AllowedCallable(foo, [int, int], {'d': int})
    # accepts a tail, kwargs complete:
    ac_bar = AllowedCallable(bar, [int, int, Tail(int)], {'d': int})

    # Not enough positional args
    raises(CannotCall, lambda : ac_foo_1(1))
    # Malformed positional arg
    raises(CannotCall, lambda: ac_foo_1(1, 'bar'))

    # Kwargs can be omitted
    assert ac_foo_1(1, 1) == 28
    # Kwargs can be given
    assert ac_foo_1(1, 1, c=1) == 22
    # Extra positional args go to kwargs, in order
    assert ac_foo_1(1, 1, 1, 2) == 14
    # Cannot absorb more extra pos args than there are kwargs
    raises(CannotCall, lambda: ac_foo_1(1, 2, 3, 4, 5))
    # Do not accept multiple values for a single kwarg
    raises(CannotCall, lambda: ac_foo_1(1, 2, 3, 4, d=4))
    # Extra positional args not allowed, when kwargs marked as incomplete
    raises(CannotCall, lambda : ac_foo_2(1, 1, 1))

    # Extra positional args converted to kwargs, when allowed
    signature = ac_foo_1.alternatives[0]
    checked_args = signature.check_args([1, 1, 1, 2], {})
    assert checked_args.A == [1, 1]
    assert checked_args.K == {'c': 1, 'd': 2}

    # If kwargs incomplete, and marked as such, Signature raises BadArgs on
    # extra pos arg.
    signature = ac_foo_2.alternatives[0]
    raises(BadArgs, lambda: signature.check_args([1, 1, 2], {}))

    # If kwargs incomplete, but dev forgot to mark it as such, still convert
    # extra positional arg to the kwarg against which it was checked.
    signature = ac_foo_3.alternatives[0]
    checked_args = signature.check_args([1, 1, 2], {})
    assert checked_args.A == [1, 1]
    assert checked_args.K == {'d': 2}

    # Tail args accepted
    assert ac_bar(1, 1, 2, 2, 2, d=5) == 41
    # Empty tail okay
    assert ac_bar(1, 1, d=5) == 23
    # Malformed tail arg
    raises(CannotCall, lambda : ac_bar(1, 1, 2, 2, '2', d=5))
    # Malformed kwarg
    raises(CannotCall, lambda: ac_bar(1, 1, d='5'))
    # Unknown kwarg
    raises(CannotCall, lambda: ac_bar(1, 1, e=5))
