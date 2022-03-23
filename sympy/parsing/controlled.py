"""Evaluation of Python ASTs, with controlled callables. """

import ast
import types

from sympy.core.function import UndefinedFunction
from sympy.core.relational import Relational
from sympy.parsing.exceptions import ControlledEvaluationException


def is_builtin_method(m):
    """
    Try to identify built-in methods.

    Observe:

    >>> type(str.join)
    method_descriptor
    >>> type('foo'.join)
    builtin_function_or_method

    >>> repr(str.join)
    "<method 'join' of 'str' objects>"
    >>> repr('foo'.join)
    '<built-in method join of str object at 0x10ba8c8f0>'

    In the technical parlance of Python, a generic method like `str.join` of
    a built-in type is a "method descriptor," while the particular method like
    `'foo'.join` belonging to an instance of a built-in type is a "built-in
    method."

    Given any object, how do you determine if it's a built-in method?

    This is a good start:

    >>> import types
    >>> type('foo'.join) == types.BuiltinMethodType
    True

    but unfortunately it does not distinguish a built-in method from a built-in
    function:

    >>> type(len) == types.BuiltinMethodType
    True

    Nor does this:

    >>> type(len) == types.BuiltinFunctionType
    True
    >>> type('foo'.join) == types.BuiltinFunctionType
    True

    However, we can consider the qualname:

    >>> 'foo'.join.__qualname__
    'str.join'
    >>> len.__qualname__
    'len'

    which should have two segments for built-in methods, but just one segment
    for built-in functions.

    We can also consult the ``__self__`` attribute, which is equal to the
    builins module for built-in functions:

    >>> s = 'foo'
    >>> s.join.__self__ is s
    True
    >>> len.__self__
    <module 'builtins' (built-in)>

    This is an important test, because when we use this function to identify
    a built-in method, we are going to be passing that method's ``__self__``
    attribute as the first argument, and it sounds risky to go passing the
    builtins module as any argument to any function (even if it would be
    blocked by argument type checking).
    """
    q = getattr(m, '__qualname__', '')
    s = getattr(m, '__self__', None)
    return (
        type(m) == types.BuiltinMethodType and
        type(s) != types.ModuleType and
        len(q.split('.')) == 2
    )


def make_key_for_callable(callable):
    """
    One of our basic safety mechanisms is that, apart from a couple of special
    cases (namely, instances of `UndefinedFunction`, or functions passed to
    a `ControlledEvaluator` as locals), we do not call any object that we are
    handed. Instead, we use the object we are handed to look up an
    ``AllowedCallable`` instance, and then we call that. Thus, we only ever
    call things that we constructed and approved in advance, not things users
    constructed.

    For this to work, we need a way to translate any given callable into a key,
    with which we can look up the corresponding AllowedCallable. For most
    types of callables, this is easy: for built-in functions, user-defined
    functions, and user-defined classes, the callable itself can be used as
    the key. For bound methods of user-defined classes, we can use the
    ``__func__`` attribute, which points to the unbound function.

    For built-in methods, however, there is no ``__func__`` attribute, and we
    instead use their ``__qualname__`` attribute (which is a string).

    In order to understand the problem, we need to contrast built-in methods
    with the methods of user-defined classes. The great thing about the latter
    is that bound methods of user-defined classes have a ``__func__`` attribute,
    which is equal to the underlying method:

    >>> class Foo:
    >>>     def bar(self):
    >>>         return 42
    >>> f = Foo()
    >>> f.bar.__func__ == Foo.bar
    True

    This means that when we're defining an ``AllowedCallable`` for ``Foo.bar``,
    we can pass ``Foo.bar`` as the callable, and later, when a user tries to
    invoke `f.bar()`, we can use `f.bar.__func__` as the key.

    This does not work with built-in methods. The analogy is that ``f.bar`` is
    to ``Foo.bar`` as ``'foo'.join`` is to ``str.join``. The latter is called
    a "method descriptor." We would like ``'foo'.join`` to have a ``__func__``
    attribute, and we would like it to be equal to ``str.join``, but this is
    not the case:

    >>> hasattr('foo'.join, '__func__')
    False

    We cannot compare to the method desriptor directly either:

    >>> 'foo'.join == str.join
    False

    Since for everything *except* built-in methods, the key is a callable (not
    a string), we can use the ``__qualname__`` of a built-in method as its key,
    without fear of this string colliding with any other key.
    """
    if is_builtin_method(callable):
        return callable.__qualname__
    elif type(callable) == types.MethodType:
        return callable.__func__
    return callable


class ControlledEvaluator(ast.NodeTransformer):
    """
    Supports a subset of the Python language, and...

    * Only allows callables meeting one of the following conditions:
        (1) instance of ``UndefinedFunction`` (an abstract function)
        (2) is a value in the given locals dict
        (3) on an explicit whitelist of functions

    * Supports limiting the size of operands to mult and pow (subclasses
      must override to implement this).
    """

    translate = {
        # Binary operators
        'Add': lambda a, b: a + b,
        'Sub': lambda a, b: a - b,
        'Mult': lambda a, b: a * b,
        'Div': lambda a, b: a / b,
        'FloorDiv': lambda a, b: a // b,
        'Mod': lambda a, b: a % b,
        'Pow': lambda a, b: a ** b,
        'LShift': lambda a, b: a << b,
        'RShift': lambda a, b: a >> b,
        'BitOr': lambda a, b: a | b,
        'BitXor': lambda a, b: a ^ b,
        'BitAnd': lambda a, b: a & b,
        'MatMult': lambda a, b: a @ b,
        # Unary operators
        'UAdd': lambda a: +a,
        'USub': lambda a: -a,
        'Not': lambda a: not a,
        'Invert': lambda a: ~a,
        # Boolean operators
        'And': lambda cs: all(cs),
        'Or': lambda ds: any(ds),
        # Comparisons
        'Eq': lambda a, b: a == b,
        'NotEq': lambda a, b: a != b,
        'Lt': lambda a, b: a < b,
        'LtE': lambda a, b: a <= b,
        'Gt': lambda a, b: a > b,
        'GtE': lambda a, b: a >= b,
        'Is': lambda a, b: a is b,
        'IsNot': lambda a, b: a is not b,
        'In': lambda a, b: a in b,
        'NotIn': lambda a, b: a not in b,
    }
    pass_through = [
        'Load',
    ]

    def __init__(self, local_dict, global_dict,
                 allow_undef_func_calls=False,
                 allow_local_dict_calls=False,
                 log_path=None):
        self.allowed_callables = {}
        self.local_dict = local_dict
        self.global_dict = global_dict
        self.allow_undef_func_calls = allow_undef_func_calls
        self.allow_local_dict_calls = allow_local_dict_calls
        self.log_path = log_path

    def add_allowed_callables(self, acs):
        """
        Add a list (or other iterable) of ``AllowedCallable``s.
        """
        for ac in acs:
            k = make_key_for_callable(ac.callable)
            self.allowed_callables[k] = ac

    def visit(self, node):
        """Visit a node."""
        method = 'visit_' + node.__class__.__name__
        with open('bar.txt', 'a') as f:
            f.write(method + '\n')
        visitor = getattr(self, method, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        classname = node.__class__.__name__
        if classname in self.pass_through:
            return node
        elif classname in self.translate:
            return self.translate[classname]
        raise ControlledEvaluationException(f'Unsupported node type, `{classname}`.')

    def recurse(self, node):
        """
        Visitor methods should start with a call to this method, in order
        to achieve a bottom-up traversal of the tree.

        Compare `ast.NodeTransformer.generic_visit`.
        """
        for field, old_value in ast.iter_fields(node):
            if isinstance(old_value, list):
                new_values = []
                for value in old_value:
                    if isinstance(value, ast.AST):
                        value = self.visit(value)
                    # Unlike in `ast.NodeTransformer.generic_visit`, which
                    # calls `extend` here, we call `append` so that `value`
                    # need not be a list.
                    new_values.append(value)
                old_value[:] = new_values
            elif isinstance(old_value, ast.AST):
                new_node = self.visit(old_value)
                # Unlike in `ast.NodeTransformer.generic_visit`, we accept
                # `None` as an actual return value, like any other.
                setattr(node, field, new_node)
        return node

    def visit_Expr(self, node):
        self.recurse(node)
        return node.value

    def visit_Call(self, node):
        self.recurse(node)
        F = node.func
        A = node.args or []
        K = {k: v for k, v in node.keywords}

        if self.log_path:
            self.log_call_attempt(F, A, K)

        if self.allow_undef_func_calls and isinstance(F, UndefinedFunction):
            return F(*A, **K)

        if self.allow_local_dict_calls and F in self.local_dict.values():
            return F(*A, **K)

        if is_builtin_method(F) or type(F) == types.MethodType:
            # When `F` is a bound method, the callable we end up calling is
            # always the unbound version. So must prepend the `self` arg.
            A = [F.__self__] + A

        key = make_key_for_callable(F)
        if key in self.allowed_callables:
            ac = self.allowed_callables[key]
            return ac(*A, **K)
        else:
            self.provide_help_for_missing_callable(key)

        raise ControlledEvaluationException(f'Disallowed callable: {getattr(F, "__module__", "<nomod>")}.{getattr(F, "__qualname__", "<noname>")}')

    def provide_help_for_missing_callable(self, key):
        """
        Subclasses may wish to override.
        This is a chance to provide the user with a helpful error message,
        in the case that a callable of a given key was not found.
        """
        pass

    def log_call_attempt(self, F, A, K):
        import traceback, json
        from sympy.parsing.allow import CheckedArgs
        with open(self.log_path, 'a') as f:
            mod = getattr(F, "__module__", "<nomod>")
            name = getattr(F, "__qualname__", "<noname>")
            class_ = str(getattr(F, "__class__", "<noclass>"))
            arg_types = [str(type(a)) for a in A]
            kwarg_types = {k:str(type(v)) for k, v in K.items()}
            stack = traceback.format_stack()

            allow = '<none>'
            if isinstance(F, UndefinedFunction):
                allow = '<ok>.<undef_func>'
            elif F in self.local_dict.values():
                allow = '<ok>.<local_dict>'
            else:
                ac = self.allowed_callables.get(F)
                if ac is None:
                    allow = '<unknown>'
                else:
                    result = ac.check_args(A, K)
                    if isinstance(result, CheckedArgs):
                        allow = '<ok>.<whitelist>'
                    else:
                        allow = f'<fail>:{result}'

            info = {
                "mod": mod, "name": name, "class": class_,
                "arg_types": arg_types, "kwarg_types": kwarg_types,
                "stack": stack,
                "allow": allow,
            }
            t = json.dumps(info)+'\n'+("%"*80)+'\n'
            f.write(t)

    def visit_Name(self, node):
        name = node.id
        if name in self.local_dict:
            return self.local_dict[name]
        if name in self.global_dict:
            return self.global_dict[name]
        raise NameError(f'Unknown name: {name}')

    def visit_Attribute(self, node):
        self.recurse(node)
        return getattr(node.value, node.attr)

    def visit_Constant(self, node):
        return node.value

    def visit_keyword(self, node):
        self.recurse(node)
        return node.arg, node.value

    def visit_BinOp(self, node):
        # First note whether it's Pow or Mult.
        is_pow = isinstance(node.op, ast.Pow)
        is_mult = isinstance(node.op, ast.Mult)
        self.recurse(node)
        L, R = node.left, node.right
        if is_pow:
            self.pow_check(L, R)
        if is_mult:
            self.mult_check(L, R)
        return node.op(L, R)

    def pow_check(self, b, e):
        """
        Subclasses may wish to override.
        This is a chance to raise an exception and prevent an operation on
        arguments that are too large.
        """
        pass

    def mult_check(self, a, b):
        """
        Subclasses may wish to override.
        This is a chance to raise an exception and prevent an operation on
        arguments that are too large.
        """
        pass

    def visit_BoolOp(self, node):
        self.recurse(node)
        return node.op(node.values)

    def visit_UnaryOp(self, node):
        self.recurse(node)
        return node.op(node.operand)

    def visit_List(self, node):
        self.recurse(node)
        return node.elts

    def visit_Tuple(self, node):
        self.recurse(node)
        return tuple(node.elts)

    def visit_Subscript(self, node):
        self.recurse(node)
        v, s = node.value, node.slice
        return v[s]

    def visit_Index(self, node):
        self.recurse(node)
        return node.value

    def visit_Slice(self, node):
        self.recurse(node)
        return slice(node.lower, node.upper, node.step)

    def visit_Compare(self, node):
        self.recurse(node)
        L = node.left
        for test, R in zip(node.ops, node.comparators):
            cmp = test(L, R)
            if isinstance(cmp, Relational):
                return cmp
            if not cmp:
                return False
            L = R
        return True

    ###########################################################################
    # Support for Python 3.7

    def visit_Str(self, node):
        return node.s

    def visit_Num(self, node):
        return node.n

    def visit_NameConstant(self, node):
        return node.value
