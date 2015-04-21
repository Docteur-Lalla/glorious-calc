(* Glorious syntax's keyword list *)
let kwds = [ "+" ; "-" ; "*" ; "/" ; "(" ; ")" ;
  "^" ; "mod" ; "," ; "=" ; ":" ; "let" ;
  "[" ; "]" ; "{" ; "}" ] ;;

let args = List.tl (Array.to_list Sys.argv) ;;

(* Concat a string with a space between them *)
let concat a b = a ^ " " ^ b ;;

let () =
  (* Create the whole formula (with declarations) by putting all arguments together *)
  let formula = List.fold_left concat (List.hd args) (List.tl args)
  in try
    let tokens = Genlex.make_lexer kwds (Stream.of_string formula)
    in let (env, ast) = Syntax.parse tokens
    in Exec.print_number (Exec.execute env ast) ; print_newline ()
  (* Exception handling *)
  with
  | Syntax.SyntaxError s -> prerr_endline ("Syntax error : " ^ s)
  | Exec.DomainError (f, exp, has) ->
    prerr_endline ("In function " ^ f ^ " : expected a " ^ exp ^ " and got a " ^ has)
  | Exec.UnknownFunction f -> prerr_endline ("Unknown function named " ^ f)
  | Exec.WrongArgumentNumber (f, exp, got) ->
    let sexp = string_of_int exp and sgot = string_of_int got
    in let str = "Function " ^ f ^ " expected " ^ sexp
    in let sarg = if exp = 0 || exp = 1 then " argument" else " arguments"
    in prerr_endline (str ^ sarg ^ " but called with " ^ sgot)
  | Stream.Failure -> prerr_endline "Syntax error"
  | Stream.Error e when e <> "" -> prerr_endline ("Syntax error : " ^ e)
  | _ -> prerr_endline "Unknown error appeared" ;;
