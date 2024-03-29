open Execute

let dataTypes_toString (obj: Execute.dataTypes) =
    match obj with
    | Int i -> print_int i
    | Bool b -> if b then print_endline "true" else print_endline "false"
    | String s -> print_endline s
    | _ -> print_endline "Error"


let _ =
    let input = open_in Sys.argv.(1) in
    let lexbuf = Lexing.from_channel input in
    let main = Parser.main Lexer.token lexbuf in
    let instr_env = 
    (fun fname -> match fname with
    | "main" -> main
    | _ -> Skip
    ) in 
    let res = compute_output instr_env  "f0" 10 in
    dataTypes_toString res