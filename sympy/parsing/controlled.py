"""Evaluation of Python ASTs, with controlled callables. """

import ast
import types

from sympy.core.function import UndefinedFunction
from sympy.core.relational import Relational
from sympy.parsing.exceptions import ControlledEvaluationException


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

    def __init__(self, allowed_callables, local_dict, global_dict, log_path=None):
        self.allowed_callables = allowed_callables
        self.local_dict = local_dict
        self.global_dict = global_dict
        self.log_path = log_path

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

        # Unbind bound methods.
        # This is so we can look for the underlying function in our list of allowed functions.
        if type(F) == types.MethodType:
            # Prepend the `self` arg to the list of args.
            self_arg = F.__self__
            A = [self_arg] + A
            # Replace the bound method with the underlying function.
            F = F.__func__

        if self.log_path:
            self.log_call_attempt(F, A, K)

        if (isinstance(F, UndefinedFunction) or (F in self.local_dict.values())):
            return F(*A, **K)
        elif F in self.allowed_callables:
            ac = self.allowed_callables[F]
            return ac(*A, **K)

        raise ControlledEvaluationException(f'Disallowed callable: {getattr(F, "__module__", "<nomod>")}.{getattr(F, "__qualname__", "<noname>")}')

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
