(* Internal representation of a number *)
type number =
| Integer of int
| Float of float
| Complex of (float * float)
;;

(* Type to make distinction between built-in functions and custom ones *)
type func =
| Sqrt
| Cos
| Sin
| Tan
| Acos
| Asin
| Atan
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

(*
 * Ast of a formula
 * There is no need to create a specific branch of the ast for declarations
 * These are treated differently
 *)
type ast =
| Leaf of number
| Op of ast * operator * ast
| Func of func * ast list
;;

(* Data type holding a custom function (declarations) *)
type data =
{
  name : string ;
  args : string list ;
  formula : ast
} ;;
