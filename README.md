# STCert

Collection of tools for proving properties on a self defined language that looks like Structured Text.

For now, we only have a proven interpreter.

## How to build

Make sure you have coq (and ocaml) available.

Then use the *Makefiles* in each directory in order: trusted, src

You should obtain a *main* executable file, which reads and execute code given in argument. You have example code in the *prog* folder.

## What's next

- [ ] Program execution
    - [x] Expressions
    - [x] Variables
    - [x] Conditions (If and Case)
    - [x] Loops (While)
    - [x] Functions (for now, call possible, but each functions share the same environment)
    - [x] Arrays
    - [x] Floats (no operations across types like int and float ...)
    - [ ] Message Sending / Recieving
    - [ ] Unkown valued variable
- [ ] Properties Proving
    - [x] Execution is equivalent to semantic
    - [ ] Hoare Logic
    - [ ] Deadlock freedom

## Directories and project structure

### trusted

The trusted folder contains all the coq files of the project.
- Syntax.v : define syntax of language
- Semantic.v : define the semantic of language
- Evalexpr.v : define how we evalue expressions
- Execute.v : define how to execute programs
- Execution_correct.v : proof that execution is equivalent to language semantic
- Export.v : Extract execution function to ocaml

### src

The src folder contains all the ocaml code, used to build the main executabe file.
- lexer.mll : define the lexer spec. The lexer is then generated with ocamllex
- parser.mly : define the parser spec. The parser is then generated with ocamlyacc
- formating.ml : format objects defined by coq to string, so we can debug
- main.ml : Read input file, use lexer and parser to build token tree, and the gives it to the proved execution function extracted from coq.