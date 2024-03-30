open Execute

let dataTypes_toString (obj: Execute.dataTypes) : string =
  match obj with
  | Int i -> "Int " ^ (string_of_int i)
  | Bool b -> if b then "Bool true" else "Bool false"
  | String s -> s
  | Array _ -> "Array printing not implemented (yet)"
  | Error -> "Error"


let rec expr_toString (obj: Execute.expr) : string = 
  match obj with
  | Const dt    -> "Const " ^ (dataTypes_toString dt)
  | Var v       -> "Var \"" ^ v ^ "\""
  | Access (a, i)  -> (dataTypes_toString a) ^ "[" ^ (string_of_int i) ^ "]"
  | Add (e1, e2)   -> "(" ^ (expr_toString e1) ^ ") + (" ^ (expr_toString e2) ^ ")"
  | Sub (e1, e2)   -> "(" ^ (expr_toString e1) ^ ") - (" ^ (expr_toString e2) ^ ")"
  | Mul (e1, e2)   -> "(" ^ (expr_toString e1) ^ ") * (" ^ (expr_toString e2) ^ ")"
  | Div (e1, e2)   -> "(" ^ (expr_toString e1) ^ ") / (" ^ (expr_toString e2) ^ ")"
  | Mod (e1, e2)   -> "(" ^ (expr_toString e1) ^ ") % (" ^ (expr_toString e2) ^ ")"
  | Exp (e1, e2)   -> "exp(" ^ (expr_toString e1) ^ ", " ^ (expr_toString e2) ^ ")"
  | And (e1, e2)   -> "(" ^ (expr_toString e1) ^ ") && (" ^ (expr_toString e2) ^ ")"
  | Or (e1, e2)    -> "(" ^ (expr_toString e1) ^ ") || (" ^ (expr_toString e2) ^ ")"
  | Not e       -> "~(" ^ (expr_toString e) ^ ")"
  | Eq0 (e1, e2)   -> "(" ^ (expr_toString e1) ^ ") == (" ^ (expr_toString e2) ^ ")"
  | Lt0 (e1, e2)   -> "(" ^ (expr_toString e1) ^ ") < (" ^ (expr_toString e2) ^ ")"
  | Lte (e1, e2)   -> "(" ^ (expr_toString e1) ^ ") <= (" ^ (expr_toString e2) ^ ")"
  | Gt0 (e1, e2)   -> "(" ^ (expr_toString e1) ^ ") > (" ^ (expr_toString e2) ^ ")"
  | Gte (e1, e2)   -> "(" ^ (expr_toString e1) ^ ") >= (" ^ (expr_toString e2) ^ ")"
  | Neq (e1, e2)   -> "(" ^ (expr_toString e1) ^ ") != (" ^ (expr_toString e2) ^ ")"

let rec stmt_toString (obj: Execute.stmt) : string =
match obj with
| Skip           -> 
  "Skip"
| Assign (v, e)  -> 
  "Assign \"" ^ v ^ "\" (" ^ (expr_toString e) ^ ")" 
| Seq ls         ->
  let func = fun sls s' -> (sls ^ " ; " ^ (stmt_toString s')) in
  "Seq [" ^ (List.fold_left func "" ls) ^ "]"
| If (c, st, sf) -> 
  "If (" ^ (expr_toString c) ^ ") (" ^ (stmt_toString st) ^ ") (" ^ (stmt_toString sf) ^ ")"
| Case (e, ls) ->
  let func = fun sls s' -> (sls ^ " ; " ^ (stmt_toString s')) in
  "Case (" ^ (expr_toString e) ^ ") [" ^ (List.fold_left func "" ls) ^ "]"
| While (e, s) -> 
  "While (" ^ (expr_toString e) ^ ") {" ^ (stmt_toString s) ^ "}"
| Call s -> "Call " ^ s