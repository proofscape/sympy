# The Alpine Mathematics fork of SymPy

[SymPy](https://github.com/sympy/sympy) is a computer algebra system written
entirely in Python. It is a powerful library, and an excellent open-source
project.

This is the fork of SymPy that is deployed in the Proofscape ISE application.

Our intention is to keep up with the latest version of SymPy, while maintaining
a few differences.

At present the main difference is in our `parse_expr()` function. For us, this
function relies on the [`displaylang`](https://github.com/alpinemath/displaylang)
package.

## Versioning

We are starting out at major version 0. This is consistent with a new project,
in which we are not committing to backwards compatibility.

We will try to keep the minor version equal to the latest minor version of
SymPy.

Our patch version will vary independently of that of SymPy. It will encompass
patches merged from SymPy, plus our own patches, as well as any features added
here that are not being added to the main SymPy project.
