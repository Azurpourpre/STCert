Require Import ZArith String List.
Require Import Syntax.

Fixpoint eval_expr (env: data_state) (e: expr) : DataTypes :=
match e with
| Const n => n
| Var v => env v
| Access a n =>
    match a with
    | Array (_,l) => nth n l Error
    | _ => Error
    end
| Add e1 e2 => 
    match (eval_expr env e1), (eval_expr env e2) with
    | Int i1, Int i2 => Int (Z.add i1 i2)
    | String s1, String s2 => String (String.append s1 s2)
    | _, _ => Error
    end
| Sub e1 e2 => 
    match (eval_expr env e1), (eval_expr env e2) with
    | Int i1, Int i2 => Int (Z.sub i1 i2)
    | _, _ => Error
    end
| Mul e1 e2 =>
    match (eval_expr env e1), (eval_expr env e2) with
    | Int i1, Int i2 => Int (Z.mul i1 i2)
    | _, _ => Error
    end
| Div e1 e2 => 
    match (eval_expr env e1), (eval_expr env e2) with
    | Int i1, Int i2 => Int (Z.div i1 i2)
    | _, _ => Error
    end
| Mod e1 e2 => 
    match (eval_expr env e1), (eval_expr env e2) with
    | Int i1, Int i2 => Int (Z.modulo i1 i2)
    | _, _ => Error
    end
| Exp e1 e2 => 
    match (eval_expr env e1), (eval_expr env e2) with
    | Int i1, Int i2 => Int (Z.pow i1 i2)
    | _, _ => Error
    end
| And e1 e2 => 
    match (eval_expr env e1), (eval_expr env e2) with
    | Int i1, Int i2 => Int (Z.land i1 i2)
    | Bool b1, Bool b2 => Bool (andb b1 b2)
    | _, _ => Error
    end
| Or e1 e2 => 
    match (eval_expr env e1), (eval_expr env e2) with
    | Int i1, Int i2 => Int (Z.lor i1 i2)
    | Bool b1, Bool b2 => Bool (orb b1 b2)
    | _, _ => Error
    end
| Not e =>
    match (eval_expr env e) with
    | Int i => Int (Z.lnot i)
    | Bool b => Bool (negb b)
    | _ => Error
    end
| Eq e1 e2 => 
    match (eval_expr env e1), (eval_expr env e2) with
    | Int i1, Int i2 => Bool (Z.eqb i1 i2)
    | Bool b1, Bool b2 => Bool (Bool.eqb b1 b2)
    | String s1, String s2 => Bool (String.eqb s1 s2)
    | _, _ => Error
    end 
| Lt e1 e2 => 
    match (eval_expr env e1), (eval_expr env e2) with
    | Int i1, Int i2 => Bool (Z.ltb i1 i2)
    | _, _ => Error
    end
| Lte e1 e2 =>
    match (eval_expr env e1), (eval_expr env e2) with
    | Int i1, Int i2 => Bool (Z.leb i1 i2)
    | _, _ => Error
    end
| Gt e1 e2 =>
    match (eval_expr env e1), (eval_expr env e2) with
    | Int i1, Int i2 => Bool (Z.gtb i1 i2)
    | _, _ => Error
    end
| Gte e1 e2 =>
    match (eval_expr env e1), (eval_expr env e2) with
    | Int i1, Int i2 => Bool (Z.geb i1 i2)
    | _, _ => Error
    end
| Neq e1 e2 =>
    match (eval_expr env e1), (eval_expr env e2) with
    | Int i1, Int i2 => Bool (negb (Z.eqb i1 i2))
    | _, _ => Error
    end
end.
