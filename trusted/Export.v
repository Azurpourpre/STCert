Require Import Syntax Execute.
Require Extraction.
Require Import ExtrOcamlZInt ExtrOcamlNativeString ExtrOcamlNatInt.

Extraction Language OCaml.

Extraction TestCompile compute_output.

Extraction "../src/execute.ml" compute_output.