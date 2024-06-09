Require Import Peano ZArith List String Lia Bool.
Import ListNotations.
Require Import Syntax Evalexpr.

Fixpoint execute (s: stmt) (data_env: data_state) (instr_env: instr_state) (wd: nat) : option data_state :=
match wd, s with
| 0, _ => None
| S new_wd, Skip => Some(data_env)
| S new_wd, Assign v e => Some(update_state data_env v (eval_expr data_env e))
| S new_wd, Seq ls => 
    match ls with 
    | [] => Some(data_env)
    | s'::ls' => 
            match (execute s' data_env instr_env new_wd) with
            | None => None
            | Some data_env' => execute (Seq ls') data_env' instr_env new_wd
            end 
    end
| S new_wd, If c st sf =>
    match (eval_expr data_env c) with
    | Bool true => execute st data_env instr_env new_wd
    | Bool false => execute sf data_env instr_env new_wd
    | _ => None
    end
| S new_wd, Case e ls =>
    match eval_expr data_env e with
    | Int z => match z with 
        | Zneg _ => None
        | _ =>
            match nth_error ls (Z.to_nat z) with
            | Some s => execute s data_env instr_env new_wd
            | None => None
            end
        end
    | _ => None
    end
| S new_wd, While e s =>
    match eval_expr data_env e with
    | Bool true => match execute s data_env instr_env new_wd with
        | Some data_env' => execute (While e s) data_env' instr_env new_wd
        | None => None
        end
    | Bool false => Some(data_env)
    | _ => None
    end
| S new_wd, Call n => execute (instr_env n) data_env instr_env new_wd
| S new_wd, Modify a n e =>
    match (data_env a), (eval_expr data_env n) with
    | Array (si, ei, l), Int (Zpos i) => if ((si <=? Pos.to_nat(i)) && (Pos.to_nat(i) <=? ei)) then 
        Some(update_state data_env a (Array (si, ei, update_array l (Pos.to_nat i) (eval_expr data_env e) ))) 
      else None
    | Array (si, ei, l), Int 0 => if ((si <=? 0) && (0 <=? ei)) then
        Some(update_state data_env a (Array (si, ei, update_array l 0 (eval_expr data_env e))))
      else None
    | _, _ => None
    end
end.


Definition compute_output (instr_env: instr_state) (rep_name: string) (wd: nat) : DataTypes :=
  let main_func : stmt := instr_env "main"%string in
    match execute main_func (fun _ => Error) instr_env wd with
    | Some env => env rep_name
    | None => Error
    end.

