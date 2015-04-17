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
| Custom of string
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
| Func of func * ast list
;;

(* Data type holding a custom function *)
type data =
{
  name : string ;
  args : string list ;
  formula : ast
} ;;
