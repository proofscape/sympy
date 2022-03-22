"""Exceptions for parsing. """


class ControlledEvaluationException(Exception):
    """Any exception during use of a ControlledEvaluator. """
    pass


class BadArgs(ControlledEvaluationException):
    """
    Represents a failure of given positional and keyword args to match a
    particular Signature for an AllowedCallable.
    """
    pass


class CannotCall(ControlledEvaluationException):
    """
    Represents a failure to call an AllowedCallable `ac`, meaning every
    Signautre raised a `BadArgs` exception.

    Stores a list of `BadArgs` exceptions, one for each alternative Signature
    of `ac`.
    """

    def __init__(self, badArgsExceps):
        self.badArgsExceps = badArgsExceps

    def __str__(self):
        return '\n'.join(str(e) for e in self.badArgsExceps)
