open Execute
open Formating

let _ =
    let input = open_in Sys.argv.(1) in
    let lexbuf = Lexing.from_channel input in
    let main = Parser.main Lexer.token lexbuf in
    let result = compute_output main Sys.argv.(2) 100 in
    print_endline (dataTypes_toString result)