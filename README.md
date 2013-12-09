Math.Diophantine
==============

A quadratic diophantine equation solving library for haskell.


Overview:
---------

This library is designed to solve for equations in the form of:

    ax^2 + bxy + cy^2 + dx + ey + f = 0

Throughout the library, the variables (a,b,c,d,e,f) will always refer to these
coefficients. This library will also use the alias:

    type Z = Integer

to shorten the type declerations of the data types and functions.


Installation:
-------------

To install the library, just use cabal along with the provided install files.


Use:
----

import the library with:

    import module Math.Diophantine

The most import function of this library is `solve :: Equation -> Solution`.
The types of equations that this library can solve are defined by the different
instances of `Equation`:

- `GeneralEquation Z Z Z Z Z Z` - where the six Integers coincide with the six
	coefficients.
- `LinearEquation Z Z Z` - where the 3 integers are d, e, and f.
- `SimpleHyperbolicEquation Z Z Z Z` - where the 3 integers are b, d, e, and
	f.
- `ElipticalEquation Z Z Z Z Z Z` -  where the six Integers coincide with the
	six coefficients.
- `ParabolicEquation Z Z Z Z Z Z` - where the six Integers coincide with the
	six coefficients.
- `HyperbolicEquation Z Z Z Z Z Z` -  where the six Integers coincide with the
  six coefficients.

For most cases, one will want to call solve with a GeneralEquation. A
GeneralEquation is used when one does not know the type of equation before hand,
or wants to take advantage of the libraries ability to detirmine what kind of
form it fits best. One can call `specializeEquation` to convert a
GeneralEquation into the best specialized equation that it matches. This
function is called within solve, so one can pass any type of function to solve.
The specific functions will try to match to a GeneralEquation if they can;
however, they will throw an error if they cannot. The error behavior exists only
because these functions should only be called directly if and only if you know
at compile time that this function will only ever recieve the proper form. One
may want to use these directly for a speed increase, or to clarify a section of
code. The solve* functions will return a Solution. Solutions are as follows:

- `ZxZ` - ZxZ is the cartesian product of Z and Z, or the set of all pairs of
  integers. This Solution denotes cases where all pairs will satisfy your
  equation, such as 0x + 0y = 0.
- `NoSolutions` - This Solution denotes that for all (x,y) in Z cross Z,
  no pair satisfies the equation.
- `SolutionSet [(Z,Z)]` - This Solution denotes that for all pairs (x,y) in this
  set, they will satisfy the given equation.


TODO:
-----

- Finish the implementation of solveHyperbolic
- Write an equation parser from a string.
