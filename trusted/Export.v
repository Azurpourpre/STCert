Require Import Syntax Execute.
Require Extraction.

Require Import ExtrOcamlBasic ExtrOcamlZInt ExtrOcamlNatInt ExtrOcamlNativeString ExtrOcamlBasic.

Extract Constant PrimFloat.float => "float".
Extract Constant PrimFloat.sub => "Float.sub".
Extract Constant PrimFloat.mul => "Float.mul".
Extract Constant PrimFloat.ltb => "fun x y -> (Float.compare x y) < 0 ".
Extract Constant PrimFloat.leb => "fun x y -> (Float.compare x y) <= 0 ".
Extract Constant PrimFloat.eqb => "fun x y -> (Float.compare x y) == 0 ".
Extract Constant PrimFloat.div => "Float.div".
Extract Constant PrimFloat.add => "Float.add".
Extraction Inline PrimFloat.float.

Extraction Language OCaml.

Extraction "../src/execute.ml" compute_output.