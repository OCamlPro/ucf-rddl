
all: rddl.cmx

%.cmx: %.ml
	ocamlfind ocamlopt -package "ocplib-json-typed,ezjsonm" -c $<

%.cmi: %.mli
	ocamlfind ocamlopt -package "ocplib-json-typed,ezjsonm" -c $<
