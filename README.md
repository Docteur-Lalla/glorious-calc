# glorious-calc
CLI calculator for genuises in a hurry
-----------
Glorious Calc provides the user a quick access to a calculator. It uses the program's arguments as a single formula to resolve. It can handle integers, real numbers and complex numbers ! Its goal is to provide the user a quick access to a computation system without having to launch a programming-language interpreter (like Python or Ruby).

The syntax of the glorious calc is similar to the classic mathematical syntax. It accepts pairs of (), [] and {} as parentheses, Basic operations are allowed (+ - * /) plus the power operation (^) and the modulo (mod). Functions have the lowest priority in calculus and do not require parentheses, so `sqrt 5 + 3` = `sqrt(5 + 3)`.

Example :

```bash
$ calc 5 + 8 - 4i
13 - 4i

$ calc 6 / [2 + i]
2.4 - 1.2i
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

Some custom functions can be created by defining a context *before* the formula.
This is made with the following syntax :

```bash
let *function-name* *argument-list* = *function-formula* : *formula*
$ calc let f x = 5i + x : f 7
7 + 5i
```

Each declaration must be separated of the following one using a comma.
Functions that take several arguments are called like the others but with their arguments separated by a comma :

```bash
$ calc let f x y = 2 + x / y, x = 4 : f x, 5
2.8
```
