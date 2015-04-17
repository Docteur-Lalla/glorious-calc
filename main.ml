let kwds = [ "+" ; "-" ; "*" ; "/" ; "(" ; ")" ; "^" ; "mod" ; "," ; "=" ; ":" ] ;;
let args = List.tl (Array.to_list Sys.argv) ;;

let concat a b = a ^ " " ^ b ;;

let func =
{
  Data.name = "f" ;
  Data.args = ["x" ; "y"] ;
  Data.formula = Data.Op (Data.Func (Data.Custom "x", []), Data.Add, Data.Func (Data.Custom "y", []))
} ;;

let () =
  let formula = List.fold_left concat (List.hd args) (List.tl args)
  in try
    let tokens = Genlex.make_lexer kwds (Stream.of_string formula)
    in let ast = Syntax.parse tokens
    in Exec.print_number (Exec.execute [func] ast) ; print_newline ()
  with
  | Exec.DomainError (f, exp, has) ->
    prerr_endline ("In function " ^ f ^ " : expected a " ^ exp ^ " and got a " ^ has)
  | Exec.UnknownFunction f -> prerr_endline ("Unknown function named " ^ f)
  | Stream.Failure -> prerr_endline "Syntax error"
  | _ -> prerr_endline "Unknown error appeared" ;;
