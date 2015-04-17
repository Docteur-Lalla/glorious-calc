type number =
| Integer of int
| Float of float
| Complex of (float * float)
;;

type func =
| Sqrt
| Cos
| Sin
| Tan
| Abs
| Ln
| Log10
| Exp
| Conj
| Re
| Im
;;

type operator =
| Add
| Mul
| Min
| Div
| Mod
| Pow
;;

type ast =
| Leaf of number
| Op of ast * operator * ast
| Func of func * ast
;;
