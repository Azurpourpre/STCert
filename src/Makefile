build: build_lexparse build_formating build_execute
	ocamlc -c main.ml
	ocamlc -o main lexer.cmo parser.cmo execute.cmo formating.cmo main.cmo 

build_execute:
	ocamlc -c execute.mli
	ocamlc -c execute.ml

build_formating: build_execute
	ocamlc -c formating.ml

build_lexparse: build_execute
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml