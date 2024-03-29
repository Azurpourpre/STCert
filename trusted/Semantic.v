Require Import List ZArith String. 
Import ListNotations.
Require Import Syntax Evalexpr.

(* We use small step semantics *)

Inductive step : stmt -> data_state -> instr_state -> stmt -> data_state -> instr_state -> Prop :=
| step_skip: forall env ienv,
    step Skip env ienv Skip env ienv
| step_assign: forall (env: data_state) (ienv: instr_state) (x: string) (e: expr),
    step (Assign x e) env ienv Skip (update_state env x (eval_expr env e)) ienv
| step_seq_initial: forall env ienv,
    step (Seq nil) env ienv Skip env ienv
| step_seq: forall ienv env env' env'' ls s,
    step s env ienv Skip env' ienv -> step (Seq ls) env' ienv Skip env'' ienv -> step (Seq (s::ls)) env ienv Skip env'' ienv
| step_if_true: forall env c st sf env' ienv,
    eval_expr env c = Bool true -> step st env ienv Skip env' ienv -> step (If c st sf) env ienv Skip env' ienv
| step_if_false: forall env env' c st sf ienv,
    eval_expr env c = Bool false -> step sf env ienv Skip env' ienv -> step (If c st sf) env ienv Skip env' ienv
| step_case: forall env env' z c se ls ienv,
    eval_expr env c = Int z -> Z.le 0 z -> Some se = nth_error ls (Z.to_nat z) -> step se env ienv Skip env' ienv -> step (Case c ls) env ienv Skip env' ienv
| step_while_true: forall env env' env'' e s ienv,
    eval_expr env e = Bool true -> step s env ienv Skip env' ienv -> step (While e s) env' ienv Skip env'' ienv -> step (While e s) env ienv Skip env'' ienv
| step_while_false: forall env e s ienv,
    eval_expr env e = Bool false -> step (While e s) env ienv Skip env ienv
| step_call: forall env env' ienv fname,
    step (ienv fname) env ienv Skip env' ienv -> step (Call fname) env ienv Skip env' ienv
.


(* Kleene Star *)
Inductive star : stmt -> data_state -> instr_state -> stmt -> data_state -> instr_state -> Prop :=
| star_refl : forall env ienv,
    star Skip env ienv Skip env ienv
| star_step : forall env s env' s' env'' s'' ienv ienv' ienv'',
    step s env ienv s' env' ienv' -> star s' env' ienv' s'' env'' ienv'' -> star s env ienv s'' env'' ienv''
.