MLS = rddl_ast.ml rddl_yojson.ml
MLIS = rddl_ast.ml rddl_yojson.mli
PACKAGES = ocplib-json-typed yojson

all: rddl.cmxa

rddl.cmxa: rddl_ast.cmx rddl_yojson.cmx
	ocamlfind ocamlopt $(patsubst %, -package %, $(PACKAGES)) $< -a -o $@

%.cmx: %.ml
	ocamlfind ocamlopt $(patsubst %, -package %, $(PACKAGES)) -c $<

%.cmi: %.mli
	ocamlfind ocamlopt $(patsubst %, -package %, $(PACKAGES)) -c $<

.depend: $(MLS) $(MLIS) Makefile
	ocamlfind ocamldep $(patsubst %, -package %, $(PACKAGES)) $(MLS) $(MLIS) > $@

-include .depend

clean:
	-rm -f *.cm* rddl *~ *.o *.a *.dll *.dylib *.so
