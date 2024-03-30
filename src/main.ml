open Execute
open Formating

let _ =
    let input = open_in Sys.argv.(1) in
    let lexbuf = Lexing.from_channel input in
    let main = Parser.main Lexer.token lexbuf in
    let instr_env = fun fname -> main in
    let result = compute_output instr_env "f0" 100 in
    print_endline (stmt_toString main) ; print_endline (dataTypes_toString result)