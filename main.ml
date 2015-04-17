let kwds = [ "+" ; "-" ; "*" ; "/" ; "(" ; ")" ; "^" ; "mod" ; "=" ; ":" ] ;;
let args = List.tl (Array.to_list Sys.argv) ;;

let concat a b = a ^ " " ^ b ;;

let () =
  let formula = List.fold_left concat (List.hd args) (List.tl args)
  in try
    let tokens = Genlex.make_lexer kwds (Stream.of_string formula)
    in let ast = Syntax.parse tokens
    in Exec.print_number (Exec.execute ast) ; print_newline ()
  with
  | Exec.DomainError (f, exp, has) ->
    prerr_endline ("In function " ^ f ^ " : expected a " ^ exp ^ " and got a " ^ has)
  | Stream.Failure -> prerr_endline "Syntax error"
  | _ -> prerr_endline "Unknown error appeared" ;;
