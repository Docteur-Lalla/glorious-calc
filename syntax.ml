open Genlex ;;

let rec parse = parser
| [< 'Ident "sqrt" ; x = parse >] -> Data.Func (Sqrt, x)
| [< 'Ident "cos" ; x = parse >] -> Data.Func (Cos, x)
| [< 'Ident "sin" ; x = parse >] -> Data.Func (Sin, x)
| [< 'Ident "tan" ; x = parse >] -> Data.Func (Tan, x)
| [< 'Ident "ln" ; x = parse >] -> Data.Func (Ln, x)
| [< 'Ident "log" ; x = parse >] -> Data.Func (Log10, x)
| [< 'Ident "exp" ; x = parse >] -> Data.Func (Exp, x)
| [< 'Ident "abs" ; x = parse >] -> Data.Func (Abs, x)
| [< 'Ident "conj" ; x = parse >] -> Data.Func (Conj, x)
| [< 'Ident "Re" ; x = parse >] -> Data.Func (Re, x)
| [< 'Ident "Im" ; x = parse >] -> Data.Func (Im, x)
| [< s = op_low >] -> s

and op_low =
  let rec aux left = parser
  | [< 'Kwd "+" ; right = op_high ; s >] -> aux (Data.Op (left, Add, right)) s
  | [< 'Kwd "-" ; right = op_high ; s >] -> aux (Data.Op (left, Min, right)) s
  | [< >] -> left

  in parser [< left = op_high ; s >] -> aux left s

and op_high =
  let rec aux left = parser
  | [< 'Kwd "*" ; right = op_pow ; s >] -> aux (Data.Op (left, Mul, right)) s
  | [< 'Kwd "/" ; right = op_pow ; s >] -> aux (Data.Op (left, Div, right)) s
  | [< 'Kwd "mod" ; right = op_pow ; s >] -> aux (Data.Op (left, Mod, right)) s
  | [< >] -> left

  in parser [< left = op_pow ; s >] -> aux left s

and op_pow =
  let rec aux left = parser
  | [< 'Kwd "^" ; right = misc ; s >] -> Data.Op (left, Pow, (aux right s))
  | [< >] -> left

  in parser [< left = misc ; s >] -> aux left s

and misc = parser
| [< 'Kwd "(" ; x = parse ; 'Kwd ")" >] -> x
| [< 'Float f ; s >] -> parse_float f s
| [< 'Int i ; s >] -> parse_int i s
| [< 'Ident "e" >] -> Data.Leaf (Data.Float (Pervasives.exp 1.0))
| [< 'Ident "i" >] -> Data.Leaf (Data.Complex (0.0, 1.0))
| [< 'Kwd "-" ; 'Ident "i" >] -> Data.Leaf (Data.Complex (0.0, -1.0))

and parse_float f = parser
[< 'Ident "i" >] -> Data.Leaf (Data.Complex (0.0, f))
| [< >] -> Data.Leaf (Data.Float f)

and parse_int i = parser
[< 'Ident "i" >] -> Data.Leaf (Data.Complex (0.0, float_of_int i))
| [< >] -> Data.Leaf (Data.Integer i) ;;
