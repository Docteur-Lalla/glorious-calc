open Genlex ;;

exception SyntaxError of string ;;

let rec parse =
  let missing_separator = "missing ':' between functions declarations and the formula"
  and missing_formula = "missing formula after the functions declarations"

  in parser
  | [< 'Kwd "let" ; decls = decl_list ;
    'Kwd ":" ?? missing_separator ;
    formula = parse_formula ?? missing_formula >] -> (decls, formula)
  | [< formula = parse_formula >] -> ([], formula)
  | [< >] -> raise (SyntaxError "unable to parse a proper formula")

and parse_formula = parser [< s = op_low >] -> s

and arguments =
  let rec aux = parser
  | [< 'Kwd "," ; formula = parse_formula ; s >] -> formula :: aux s
  | [< >] -> []

  in parser [< formula = parse_formula ; s >] -> formula :: aux s

and decl =
  let eq_op_missing fname = "no '=' operator in " ^ fname ^ " declaration"

  in parser
    [< 'Ident fname ; args = parameters ; 'Kwd "=" ?? eq_op_missing fname ; formula = parse_formula >] ->
    { Data.name = fname ; Data.args = args ; Data.formula = formula }

and parameters = parser
| [< 'Ident id ; s >] -> id :: parameters s
| [< >] -> []

and decl_list =
  let rec aux = parser
  | [< 'Kwd "," ; dec = decl ; s >] -> dec :: aux s
  | [< >] -> []

  in parser
  | [< dec = decl ; s >] -> dec :: aux s

and op_low = parser [< left = op_high ; s >] -> op_low_aux left s

and op_low_aux left = parser
| [< 'Kwd "+" ; right = op_high ; s >] -> op_low_aux (Data.Op (left, Add, right)) s
| [< 'Kwd "-" ; right = op_high ; s >] -> op_low_aux (Data.Op (left, Min, right)) s
| [< >] -> left

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
| [< 'Kwd "(" ; x = parse_formula ; 'Kwd ")" >] -> x
| [< 'Kwd "[" ; x = parse_formula ; 'Kwd "]" >] -> x
| [< 'Kwd "{" ; x = parse_formula ; 'Kwd "}" >] -> x
| [< 'Float f ; s >] -> parse_float f s
| [< 'Int i ; s >] -> parse_int i s
| [< 'Ident "e" >] -> Data.Leaf (Data.Float (Pervasives.exp 1.0))
| [< 'Ident "i" >] -> Data.Leaf (Data.Complex (0.0, 1.0))
| [< 'Ident id ; s >] -> (parse_function [< 'Ident id ; s >])
| [< 'Kwd "-" ; x = parse_formula >] -> Data.Op (Data.Leaf (Data.Integer 0), Min, x)
(*| [< 'Kwd "-" ; 'Ident "i" >] -> Data.Leaf (Data.Complex (0.0, -1.0))*)

and parse_function = parser
| [< 'Ident "sqrt" ; x = parse_formula >] -> Data.Func (Sqrt, [x])
| [< 'Ident "acos" ; x = parse_formula >] -> Data.Func (Acos, [x])
| [< 'Ident "asin" ; x = parse_formula >] -> Data.Func (Asin, [x])
| [< 'Ident "atan" ; x = parse_formula >] -> Data.Func (Atan, [x])
| [< 'Ident "cos" ; x = parse_formula >] -> Data.Func (Cos, [x])
| [< 'Ident "sin" ; x = parse_formula >] -> Data.Func (Sin, [x])
| [< 'Ident "tan" ; x = parse_formula >] -> Data.Func (Tan, [x])
| [< 'Ident "ln" ; x = parse_formula >] -> Data.Func (Ln, [x])
| [< 'Ident "log" ; x = parse_formula >] -> Data.Func (Log10, [x])
| [< 'Ident "exp" ; x = parse_formula >] -> Data.Func (Exp, [x])
| [< 'Ident "abs" ; x = parse_formula >] -> Data.Func (Abs, [x])
| [< 'Ident "conj" ; x = parse_formula >] -> Data.Func (Conj, [x])
| [< 'Ident "Re" ; x = parse_formula >] -> Data.Func (Re, [x])
| [< 'Ident "Im" ; x = parse_formula >] -> Data.Func (Im, [x])
| [< 'Ident id ; s >] -> parse_custom_function id s

and parse_custom_function id = parser
| [< args = arguments >] -> Data.Func (Custom id, args)
| [< op = op_low_aux (Data.Func (Custom id, [])) >] -> op

and parse_float f = parser
[< 'Ident "i" >] -> Data.Leaf (Data.Complex (0.0, f))
| [< >] -> Data.Leaf (Data.Float f)

and parse_int i = parser
[< 'Ident "i" >] -> Data.Leaf (Data.Complex (0.0, float_of_int i))
| [< >] -> Data.Leaf (Data.Integer i) ;;
