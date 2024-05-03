Require Import String ZArith Floats.

Definition var_eq := string_dec.

(* Introducing DataTypes*)
Inductive DataTypes : Set :=
| Int      (z: Z)
| Bool     (z: bool)
| String   (z: string)
| Float    (z: float)
| Array    (z: (nat * nat * (nat -> DataTypes)))
| Error
.

(*  
    Date not implemented
    Enum not implemented
    Struct not implemented
*)

(* Variables and state *)

Definition data_state := string -> DataTypes.

Definition update_state (env: data_state) (x: string) (v: DataTypes) : data_state :=
    (fun y => if var_eq y x then v else env y).

(* Expressions *)
Inductive expr : Set := 
| Const (n: DataTypes)
| Var   (v: string)

| Add   (e1 e2: expr)
| Sub   (e1 e2: expr)
| Mul   (e1 e2: expr)
| Div   (e1 e2: expr)
| Mod   (e1 e2: expr)
| Exp   (e1 e2: expr)

| And   (e1 e2: expr)
| Or    (e1 e2: expr)
| Not   (e: expr)

| Eq  (e1 e2: expr)
| Lt  (e1 e2: expr)
| Lte (e1 e2: expr)
| Gt  (e1 e2: expr)
| Gte (e1 e2: expr)
| Neq (e1 e2: expr)

| Access (a: expr) (n: expr)
.

Inductive stmt :=
| Skip
| Assign (v: string) (e: expr)
| Seq    (ls: list stmt)
| If     (c: expr) (st sf: stmt)
| Case   (e: expr) (ls: list stmt)
| While  (e: expr) (s: stmt)
| Call   (n: string)
| Modify (a: string) (n:expr) (e: expr)
.

Definition instr_state := string -> stmt.