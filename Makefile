
toplevel: typechecker.ml
	ocamlc -c typechecker.ml
	ocamlfind ocamlmktop -o top typechecker.cmo

exe: typechecker.ml
	ocamlc -c typechecker.ml
	ocamlfind ocamlc -o tc typechecker.cmo
