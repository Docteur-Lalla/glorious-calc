# glorious-calc
CLI calculator for genuises in a hurry
-----------
Glorious Calc provides the user a quick access to a calculator. It uses the program's arguments as a single formula to resolve. It can handle integers, real numbers and complex numbers !

Example :

```bash
$ calc 5 + 8 - 4i
```

-----------

Glorious Calc has some built-in functions accessible directly.

* sqrt : square root
* cos : cosine
* sin : sine
* tan : tangent
* abs : absolute value
* ln : natural logarithm
* log : base 10 logarithm
* exp : exponential
* conj : conjugate complex
* Re : real part
* Im : Imaginary part

Functions have the lowest priority in calculus and do not require parentheses, so sqrt 5 + 3 = sqrt(5 + 3).
In addition, the following operators are available : + - * / ^ (power) mod (modulo).

Some custom functions can be created by defining a context *before* the formula.
This is made due to the following syntax :

```bash
*function-name* *argument-list* = *formula* : *formula
$ calc f x = 5i + x, y = 8 : f y
```

Each declaration must be separated of the following one using a comma.
Functions that take several arguments are called like the others but with their arguments separated by a comma :

```bash
$ calc f x y = 2 + x * y : f 4, 5
```
