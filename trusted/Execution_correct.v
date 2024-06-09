Require Import Peano ZArith Znat List String Lia.
Include Nat2Z.
Import ListNotations.
Require Import Syntax Semantic Execute Evalexpr.

Lemma exec_chain:
    forall env env'' s s' wd ienv,
    match execute s env ienv wd with
    | Some env' => execute s' env' ienv wd
    | None => None
    end = Some env'' -> exists env', execute s env ienv wd = Some env' /\ execute s' env' ienv wd = Some env''. 
Proof.
    intros.
    destruct (execute s env ienv wd) eqn: exec1_res.
    - exists d. auto.
    - easy.
Qed.

Lemma exec_success_wd_pos:
    forall env env' s wd ienv, 
    execute s env ienv wd = Some env' -> exists pred, wd = S pred.
Proof.
    destruct wd ; intros.
    - simpl in H. easy.
    - exists wd. reflexivity.
Qed.

Lemma exec_success_do:
    forall env env' ienv s wd,
    execute s env ienv wd = Some env' -> exists pred, execute s env ienv (S pred) = Some env'.
Proof.
    intros.
    pose proof H as H1.
    apply exec_success_wd_pos in H. destruct H.
    exists x. rewrite <- H. apply H1.
Qed.

Lemma weak_exec_more_wd:
    forall wd s env env' ienv,
    execute s env ienv wd = Some env' -> execute s env ienv (S wd) = Some env'.
Proof. 
    induction wd.
    easy.
    destruct s ; simpl ; intros.
    - apply H.
    - apply H.
    - destruct ls eqn:ls_comp. apply H.
      apply exec_chain in H. destruct H. destruct H.
      apply IHwd in H. apply IHwd in H0. simpl in H. simpl in H0.
      destruct s ; try (rewrite H ; apply H0).
      + inversion H. apply H0.
      + inversion H. rewrite H2. apply H0.  
    - destruct (eval_expr env c) ; auto.
      destruct z ; apply IHwd ; apply H.
    - destruct (eval_expr env e) ; auto.
      destruct (nth_error ls (Z.to_nat z)) ; auto.
      destruct z.
      apply IHwd. apply H.
      apply IHwd. apply H.
      easy.
    - destruct (eval_expr env e) ; auto.
      destruct z ; auto.
      apply exec_chain in H. destruct H. destruct H.
      apply IHwd in H. apply IHwd in H0. simpl in H. simpl in H0.
      destruct s ; try (rewrite H ; apply H0).
      + inversion H. apply H0.
      + inversion H. rewrite H2. apply H0.
    - apply IHwd in H. apply H.
    - apply H.
Qed.

Lemma exec_more_wd:
    forall wd0 s env env' ienv,
    execute s env ienv wd0 = Some env' -> forall wd1, wd1 >= wd0 -> execute s env ienv wd1 = Some env'.
Proof.
    intros.
    induction H0.
    - apply H.
    - apply weak_exec_more_wd. apply IHle.  
Qed.

Lemma execute_correct_l:
    forall s env env' ienv,
    step s env ienv Skip env' ienv -> exists wd, execute s env ienv wd = Some env'.
Proof.
    intros. induction H.
    - exists 1. simpl. reflexivity.
    - exists 1. simpl. reflexivity.
    - exists 1. simpl. reflexivity.
    - destruct IHstep1. destruct IHstep2.
      exists (S (max x x0)). simpl.
      eapply exec_more_wd in H1. rewrite H1.
      eapply exec_more_wd in H2. rewrite H2.
      reflexivity. lia. lia.
    - destruct IHstep.
      exists (S x). simpl.
      rewrite H. apply H1.
    - destruct IHstep.
      exists (S x). simpl.
      rewrite H. apply H1.
    - destruct IHstep.
      exists (S x). simpl.
      rewrite H. rewrite Nat2Z.id.
      destruct (Z.of_nat n) eqn: nz_val.
      + rewrite <- H0. apply H2.
      + rewrite <- H0. apply H2.
      + lia.
    - destruct IHstep1. destruct IHstep2.
      exists (S (max x x0)). simpl.
      rewrite H.
      eapply exec_more_wd in H2. eapply exec_more_wd in H3.
      rewrite H2. rewrite H3. reflexivity. lia. lia.
    - exists 1. simpl. rewrite H. reflexivity.
    - destruct IHstep.
      exists (S x). simpl. apply H0.
    - exists 1. simpl. rewrite H. rewrite H0.
      destruct (Z.of_nat i) eqn: iz_val.
      + rewrite <- inj_0 in iz_val. apply inj in iz_val.
        rewrite iz_val in H1. rewrite <- Nat.leb_le in H1. rewrite H1. simpl. rewrite iz_val. reflexivity.
      + rewrite <- positive_nat_Z in iz_val. apply inj in iz_val. rewrite <- iz_val.
        rewrite <- Nat.leb_le in H1. rewrite H1. rewrite <- Nat.leb_le in H2. rewrite H2. simpl.
        reflexivity.
      + lia.
Qed.
      

Lemma execute_correct_r:
    forall wd s env env' ienv,
    execute s env ienv wd = Some env' -> step s env ienv Skip env' ienv.
Proof.
    induction wd ; induction s ; try easy ; simpl ; intros.
    - inversion H. apply step_skip.
    - inversion H. apply step_assign.
    - destruct ls.
      + inversion H. apply step_seq_initial.
      + apply exec_chain in H. destruct H. destruct H.
        eapply step_seq ; apply IHwd. apply H. apply H0.
    - destruct (eval_expr env c) eqn:c_val ; try easy.
      destruct z.
      + apply step_if_true. apply c_val. apply IHs1. apply weak_exec_more_wd. apply H.
      + apply step_if_false. apply c_val. apply IHs2. apply weak_exec_more_wd. apply H.
    - destruct (eval_expr env e) eqn: e_val ; try easy.
      destruct (nth_error ls (Z.to_nat z)) eqn:nth_val ; try easy.
      destruct z eqn:z_val. 
      eapply step_case. rewrite inj_0. apply e_val. simpl in nth_val. simpl. rewrite nth_val. reflexivity. apply IHwd. apply H.
      eapply step_case. rewrite (positive_nat_Z p). apply e_val. simpl in nth_val. rewrite nth_val. reflexivity. apply IHwd. apply H.
      easy.
      destruct z ; easy.
    - destruct (eval_expr env e) eqn:e_val ; try easy.
      destruct z.
      + apply exec_chain in H. destruct H. destruct H. 
        eapply step_while_true. apply e_val. apply IHwd. apply H. apply IHwd. apply H0.
      + inversion H. subst. apply step_while_false. apply e_val.
    - apply step_call. apply IHwd. apply H.
    - destruct (env a) eqn: a_val ; try easy.
      destruct z as (p, l). destruct p as (si, ei).
      destruct (eval_expr env n) eqn: n_val ; try easy.
      destruct z.
      + destruct (si <=? 0) eqn: si_sign; simpl in H.
        inversion H. apply step_modify. apply n_val. apply a_val. apply Nat.leb_le. apply si_sign. lia.   
        easy.
      + destruct (si <=? Pos.to_nat p) eqn: si_sign ; destruct (Pos.to_nat p <=? ei) eqn: ei_sign ; simpl in H ; try easy.
        inversion H. apply step_modify. rewrite positive_nat_Z. apply n_val. apply a_val. apply Nat.leb_le. apply si_sign. apply Nat.leb_le. apply ei_sign.
      + easy. 
Qed.

Theorem execute_correct:
    forall s env env' ienv,
    (exists wd, execute s env ienv wd = Some env') <-> step s env ienv Skip env' ienv.
Proof.
    intros.
    split.
    - intros. destruct H. eapply execute_correct_r. apply H.
    - apply execute_correct_l.
Qed.

Print Assumptions execute_correct.