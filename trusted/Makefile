all: build_correctness extract build_hoare

build_evalexpr: build_syntax
	coqc Evalexpr.v

build_execute: build_syntax build_evalexpr
	coqc Execute.v

build_semantic: build_syntax build_evalexpr
	coqc Semantic.v

build_syntax:
	coqc Syntax.v

build_correctness: build_syntax build_semantic build_evalexpr build_execute
	coqc Execution_correct.v > Axioms/correctness.txt

extract: build_execute
	coqc Export.v

build_hoare: build_syntax build_semantic
	coqc Hoare.v
