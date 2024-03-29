build_trusted:
	coqc -w all defs/Syntax.v > tmp/Syntax.vo
	coqc -w all src/Evalexpr.v