
all: rddl

rddl: rddl.cmx
	ocamlfind ocamlopt -package "ocplib-json-typed,ezjsonm" $< -o $@ -linkpkg

%.cmx: %.ml
	ocamlfind ocamlopt -package "ocplib-json-typed,ezjsonm" -c $<

%.cmi: %.mli
	ocamlfind ocamlopt -package "ocplib-json-typed,ezjsonm" -c $<

clean:
	-rm -f *.cm* rddl *~ *.o
