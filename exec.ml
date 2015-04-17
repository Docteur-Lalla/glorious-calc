exception DomainError of string * string * string ;;
exception UnknownFunction of string ;;

let do_to_int f n = f (float_of_int n) ;;

let sqrt = function
| Data.Integer n -> Data.Float (do_to_int Pervasives.sqrt n)
| Data.Float f -> Data.Float (Pervasives.sqrt f)
| Data.Complex _ -> raise (DomainError ("sqrt", "real", "complex")) ;;

let cos = function
| Data.Integer n -> Data.Float (do_to_int Pervasives.cos n)
| Data.Float f -> Data.Float (Pervasives.cos f)
| Data.Complex _ -> raise (DomainError ("cos", "real", "complex")) ;;

let sin = function
| Data.Integer n -> Data.Float (do_to_int Pervasives.sin n)
| Data.Float f -> Data.Float (Pervasives.sin f)
| Data.Complex _ -> raise (DomainError ("sin", "real", "complex")) ;;

let tan = function
| Data.Integer n -> Data.Float (do_to_int Pervasives.tan n)
| Data.Float f -> Data.Float (Pervasives.tan f)
| Data.Complex _ -> raise (DomainError ("tan", "real", "complex")) ;;

let abs = function
| Data.Integer n -> Data.Integer (Pervasives.abs n)
| Data.Float f -> Data.Float (Pervasives.abs_float f)
| Data.Complex (a, b) -> Data.Complex (Pervasives.abs_float a, Pervasives.abs_float b) ;;

let ln = function
| Data.Integer n -> Data.Float (do_to_int Pervasives.log n)
| Data.Float f -> Data.Float (Pervasives.log f)
| Data.Complex _ -> raise (DomainError ("ln", "real", "complex")) ;;

let log10 = function
| Data.Integer n -> Data.Float (do_to_int Pervasives.log10 n)
| Data.Float f -> Data.Float (Pervasives.log10 f)
| Data.Complex _ -> raise (DomainError ("log", "real", "complex")) ;;

let exp = function
| Data.Integer n -> Data.Float (do_to_int Pervasives.exp n)
| Data.Float f -> Data.Float (Pervasives.exp f)
| Data.Complex _ -> raise (DomainError ("exp", "real", "complex")) ;;

let conj = function
| Data.Integer n -> Data.Integer n
| Data.Float f -> Data.Float f
| Data.Complex (r, i) -> Data.Complex (r, -.i) ;;

let re = function
| Data.Integer n -> Data.Integer n
| Data.Float f -> Data.Float f
| Data.Complex (r,_) -> Data.Float r ;;

let im = function
| Data.Integer n -> Data.Integer 0
| Data.Float f -> Data.Float 0.0
| Data.Complex (_,i) -> Data.Float i ;;

let rec print_number = function
| Data.Integer n -> print_int n
| Data.Float f when f = floor f -> print_int (int_of_float f)
| Data.Float f -> print_float f
| Data.Complex (a, b) ->
  begin match a, b with
  | 0.0, 0.0 -> print_int 0
  | 0.0, 1.0 -> print_string "i"
  | 0.0, -1.0 -> print_string "-i"
  | r, 0.0 -> print_number (Data.Float r)
  | 0.0, i -> print_number (Data.Float i) ; print_string "i"
  | r, i when i < 0.0 ->
    begin
      print_number (Data.Float r) ;
      print_string " - " ;
      print_number (Data.Float (Pervasives.abs_float i)) ;
      print_string "i"
    end
  | r, i -> print_number (Data.Float r) ; print_string " + " ; print_number (Data.Float i) ; print_string "i"
  end ;;

let add = function
| Data.Integer a, Data.Integer b -> Data.Integer (a + b)
| Data.Float a, Data.Float b -> Data.Float (a +. b)
| Data.Integer a, Data.Float b -> Data.Float (float_of_int a +. b)
| Data.Float a, Data.Integer b -> Data.Float (float_of_int b +. a)
| Data.Integer n, Data.Complex (a, b) -> Data.Complex (float_of_int n +. a, b)
| Data.Complex (a, b), Data.Integer n -> Data.Complex (float_of_int n +. a, b)
| Data.Float n, Data.Complex (a, b) -> Data.Complex (n +. a, b)
| Data.Complex (a, b), Data.Float n -> Data.Complex (n +. a, b)
| Data.Complex (a, b), Data.Complex (x, y) -> Data.Complex (a +. x, b +. y) ;;

let min = function
| Data.Integer a, Data.Integer b -> Data.Integer (a - b)
| Data.Float a, Data.Float b -> Data.Float (a -. b)
| Data.Integer a, Data.Float b -> Data.Float (float_of_int a -. b)
| Data.Float a, Data.Integer b -> Data.Float (float_of_int b -. a)
| Data.Integer n, Data.Complex (a, b) -> Data.Complex (float_of_int n -. a, 0.0 -. b)
| Data.Complex (a, b), Data.Integer n -> Data.Complex (a -. float_of_int n, b)
| Data.Float n, Data.Complex (a, b) -> Data.Complex (n -. a, 0.0 -. b)
| Data.Complex (a, b), Data.Float n -> Data.Complex (a -. n, b)
| Data.Complex (a, b), Data.Complex (x, y) -> Data.Complex (a -. x, b -. y) ;;

let mul = function
| Data.Integer a, Data.Integer b -> Data.Integer (a * b)
| Data.Float a, Data.Float b -> Data.Float (a *. b)
| Data.Integer a, Data.Float b -> Data.Float (float_of_int a *. b)
| Data.Float a, Data.Integer b -> Data.Float (float_of_int b *. a)
| Data.Integer n, Data.Complex (a, b) -> let fn = float_of_int n
  in Data.Complex (fn *. a, fn *. b)
| Data.Complex (a, b), Data.Integer n -> let fn = float_of_int n
  in Data.Complex (fn *. a, fn *. b)
| Data.Float n, Data.Complex (a, b) -> Data.Complex (n *. a, n *. b)
| Data.Complex (a, b), Data.Float n -> Data.Complex (n *. a, n *. b)
| Data.Complex (a, b), Data.Complex (x, y) -> let r = a *. x -. b *. y and i = a *. y +. b *. x
  in Data.Complex (r, i) ;;

let rec div = function
| Data.Integer a, Data.Integer b -> Data.Float (float_of_int a /. float_of_int b)
| Data.Float a, Data.Float b -> Data.Float (a /. b)
| Data.Integer a, Data.Float b -> Data.Float (float_of_int a /. b)
| Data.Float a, Data.Integer b -> Data.Float (a /. float_of_int b)
| Data.Integer n, Data.Complex (a, b) -> let conj = Data.Complex (a, -.b) and den =  a*.a +. b*.b
  in mul ((Data.Float (float_of_int n /. den)), conj)
| Data.Complex (a, b), Data.Integer n -> Data.Complex (a /. float_of_int n, b /. float_of_int n)
| Data.Float n, Data.Complex (a, b) -> let conj = Data.Complex (a, -.b) and den =  a*.a +. b*.b
  in mul ((Data.Float (n /. den)), conj)
| Data.Complex (a, b), Data.Float n -> Data.Complex (a /. n, b /. n)
| Data.Complex (a, b) as z, Data.Complex (x, y) -> let conj = Data.Complex (x, -.y) and den = x*.x +. y*.y
  in let mult = mul (z, conj)
  in div (mult, (Data.Float den)) ;;

let mod_ = function
| Data.Integer a, Data.Integer b -> Data.Integer (a mod b)
| Data.Float a, Data.Integer b -> raise (DomainError ("mod", "2 integers", "a real and an integer"))
| Data.Integer a, Data.Float b -> raise (DomainError ("mod", "2 integers", "an integer and a real"))
| Data.Float a, Data.Float b -> raise (DomainError ("mod", "2 integers", "2 reals"))
| Data.Complex a, Data.Integer b -> raise (DomainError ("mod", "2 integers", "a complex and a integer"))
| Data.Integer a, Data.Complex b -> raise (DomainError ("mod", "2 integers", "an integer and a complex"))
| Data.Float a, Data.Complex b -> raise (DomainError ("mod", "2 integers", "a real and a complex"))
| Data.Complex a, Data.Float b -> raise (DomainError ("mod", "2 integers", "a complex and a real"))
| Data.Complex a, Data.Complex b -> raise (DomainError ("mod", "2 integers", "2 complex")) ;;

let pow = function
| Data.Integer a, Data.Integer b -> Data.Integer (int_of_float (float_of_int a ** float_of_int b))
| Data.Float a, Data.Float b -> Data.Float (a ** b)
| Data.Integer a, Data.Float b -> Data.Float (float_of_int a ** b)
| Data.Float a, Data.Integer b -> Data.Float (a ** float_of_int b)
(* | Data.Integer n, Data.Complex (a, b) -> Data.Complex (float_of_int n /. a, b)
| Data.Complex (a, b), Data.Integer n -> Data.Complex (float_of_int n /. a, b)
| Data.Float n, Data.Complex (a, b) -> Data.Complex (n /. a, b)
| Data.Complex (a, b), Data.Float n -> Data.Complex (n /. a, b)
| Data.Complex (a, b), Data.Complex (x, y) -> Data.Complex (a /. x, b /. y) *) ;;

let rec execute env = function
| Data.Leaf n -> n
| Data.Op (l, o, r) -> execute env (Data.Leaf (execute_operation env l o r))
| Data.Func (f, args) -> execute env (Data.Leaf (apply_function env f (List.map (execute env) args)))

and apply_function env f args = match f with
| Data.Sqrt -> sqrt (List.hd args)
| Data.Cos -> cos (List.hd args)
| Data.Sin -> sin (List.hd args)
| Data.Tan -> tan (List.hd args)
| Data.Abs -> abs (List.hd args)
| Data.Ln -> ln (List.hd args)
| Data.Log10 -> log10 (List.hd args)
| Data.Exp -> exp (List.hd args)
| Data.Conj -> conj (List.hd args)
| Data.Re -> re (List.hd args)
| Data.Im -> im (List.hd args)
| Data.Custom f -> apply_custom_function env f args

and execute_operation env l o r = match o with
| Data.Add -> add (execute env l, execute env r)
| Data.Min -> min (execute env l, execute env r)
| Data.Mul -> mul (execute env l, execute env r)
| Data.Div -> div (execute env l, execute env r)
| Data.Mod -> mod_ (execute env l, execute env r)
| Data.Pow -> pow (execute env l, execute env r)

(* Coming soon *)
and apply_custom_function env f args =
  try
    let func = List.find (fun (fdata : Data.data) -> fdata.name = f) env
    in let rec aux formula = function
    | [] -> execute env formula
    | (name, value)::t -> let new_formula = reduce name value formula
      in aux new_formula t

    and reduce name value = function
    | Data.Func (Data.Custom f, _) as func -> if f = name then Data.Leaf value else func
    | Data.Op (l, o, r) -> Data.Op (reduce name value l, o, reduce name value r)
    | leaf -> leaf
  
    in let ctx = List.map2 (fun a b -> a, b) func.args args
    in aux func.formula ctx
  with
    Not_found -> raise (UnknownFunction f) ;;
