Require Import Syntax Execute.
Require Extraction.

Require Import ExtrOcamlBasic ExtrOcamlZInt ExtrOcamlNatInt ExtrOcamlNativeString ExtrOcamlBasic.

Extract Constant PrimFloat.float => "Float".
Extraction Inline PrimFloat.float.
Extract Constant PrimFloat.sub => "Float.sub".
Extract Constant PrimFloat.mul => "Float.mul".
Extract Constant PrimFloat.ltb => "Float.ltb".
Extract Constant PrimFloat.leb => "Float.leb".
Extract Constant PrimFloat.eqb => "Float.eqb".
Extract Constant PrimFloat.div => "Float.div".
Extract Constant PrimFloat.add => "Float.add".

Extraction Language OCaml.

Extraction "../src/execute.ml" compute_output.