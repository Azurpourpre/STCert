Require Import Equality List ZArith.
Require Import Syntax Semantic Evalexpr.

Definition pred := data_state -> Prop.

Definition valid_hoare_triple (senv: instr_state) (P: pred) (s: stmt) (Q: pred) : Prop :=
    forall env1 env2, P env1 -> step s env1 senv Skip env2 senv -> Q env2.

Theorem hoare_skip: forall senv P, valid_hoare_triple senv P Skip P.
Proof.
    unfold valid_hoare_triple. intros.
    inversion H0. subst. apply H.
Qed.

Theorem hoare_assign:
    forall senv P x e,
    valid_hoare_triple senv (fun env => P (update_state env x (eval_expr env e))) (Assign x e) P.
Proof.
    unfold valid_hoare_triple. intros.
    inversion H0. subst. apply H.
Qed.

Theorem hoare_comp:
    forall senv P Q R s ls, valid_hoare_triple senv P s Q -> valid_hoare_triple senv Q (Seq ls) R
    -> valid_hoare_triple senv P (Seq (s :: ls)) R.
Proof.
    unfold valid_hoare_triple. intros.
    inversion H2. subst.
    eapply H0. eapply H.
    apply H1. apply H5. apply H8.
Qed.

Theorem hoare_seq_nil:
    forall senv P, valid_hoare_triple senv P (Seq nil) P.
Proof.
    unfold valid_hoare_triple. intros.
    inversion H0. subst. apply H.
Qed.

Theorem hoare_if:
    forall senv c st sf P Q,
    valid_hoare_triple senv (fun env => (eval_expr env c = Bool true) /\ P env) st Q ->
    valid_hoare_triple senv (fun env => (eval_expr env c = Bool false) /\ P env) sf Q ->
    valid_hoare_triple senv P (If c st sf) Q.
Proof.
    unfold valid_hoare_triple.
    intros.
    inversion H2 ; subst.
    - eapply H. split. apply H8. apply H1. apply H11.
    - eapply H0. split. apply H8. apply H1. apply H11.
Qed.

Theorem hoare_conseq:
    forall senv P1 P2 s Q1 Q2,
    (forall env, P1 env -> P2 env) -> valid_hoare_triple senv P2 s Q2 -> (forall env, Q2 env -> Q1 env) ->
    valid_hoare_triple senv P1 s Q1.
Proof.
    unfold valid_hoare_triple. intros.
    apply H1. eapply H0. apply H. apply H2. apply H3.
Qed.

Theorem hoare_while:
    forall senv P e s,
    valid_hoare_triple senv (fun env => (eval_expr env e = Bool true) /\ P env) s P -> 
    valid_hoare_triple senv P (While e s) (fun env => (eval_expr env e) = Bool false /\ P env).
Proof.
    unfold valid_hoare_triple. intros.
    dependent induction H1.
    - eapply IHstep2 ; simpl ; intros ; auto.
      + eapply H. apply H2. apply H3.
      + eapply H. split. apply H1. apply H0. apply H1_.
    - split. apply H1. apply H0.
Qed.

Theorem hoare_call:
    forall fn P Q senv,
    valid_hoare_triple senv P (senv fn) Q -> valid_hoare_triple senv P (Call fn) Q.
Proof.
    unfold valid_hoare_triple. intros.
    inversion H1. subst.
    eapply H. apply H0. apply H3.
Qed.
     