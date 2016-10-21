MLS = \
  rddl_ast.ml \
  rddl_profile.ml \
  rddl_checker.ml \
  rddl_yojson.ml \
  rddl_renderer.ml \
  rddl_renderer_builtins.ml \
  rddl_renderer_demo.ml \
  rddl_profiler.ml \
  rddl_profiler_demo.ml \
  rddl_checker_main.ml
MLIS = \
  rddl_ast.ml \
  rddl_profile.mli \
  rddl_checker.mli \
  rddl_yojson.mli \
  rddl_renderer.mli \
  rddl_profiler.mli
PACKAGES = yojson ocplib-json-typed
WITH_JS_PACKAGE = \
  rddl-profiler-demo.byte \
  rddl-renderer-demo.byte \
  rddl_client.cma
WITH_JS_SYNTAX = \
  rddl_profiler.cmo \
  rddl_renderer.cmo \
  rddl_profiler_demo.cmo \
  rddl_renderer_demo.cmo \
  rddl_renderer_builtins.cmo \
  .depend

OPTIONS = $(patsubst %, -package %, $(PACKAGES))
$(WITH_JS_PACKAGE): PACKAGES+=js_of_ocaml js_of_ocaml.tyxml ocplib-json-typed.browser
$(WITH_JS_SYNTAX): PACKAGES+=js_of_ocaml.ppx

all: \
	rddl-checker \
  rddl.cma \
  rddl.cmxa \
  rddl_client.cma \
  rddl-profiler-demo.js \
  rddl-renderer-demo.js \
  rddl_schema.json

rddl_schema.json: rddl.cma
	echo 'Rddl_yojson.schema_to_file "rddl_schema.json";;' | \
	ocaml $(shell ocamlfind -query -r -predicates byte -i-format -a-format $(PACKAGES)) rddl.cma > /dev/null

rddl-profiler-demo.byte: \
  rddl_client.cma \
  rddl_profiler_demo.cmo
	ocamlfind ocamlc -g $(OPTIONS) -linkpkg $^ -o $@

rddl-renderer-demo.byte: \
  rddl_client.cma \
  rddl_renderer_demo.cmo
	ocamlfind ocamlc -g $(OPTIONS) -linkpkg $^ -o $@

rddl-checker: \
	rddl.cmxa \
	rddl_checker_main.cmx
	ocamlfind ocamlopt -g $(OPTIONS) -linkpkg $^ -o $@

rddl.cmxa: \
  rddl_ast.cmx \
  rddl_profile.cmx \
  rddl_checker.cmx \
  rddl_yojson.cmx
	ocamlfind ocamlopt $(OPTIONS) $^ -a -o $@

rddl.cma: \
  rddl_ast.cmo \
  rddl_profile.cmo \
  rddl_checker.cmo \
  rddl_yojson.cmo
	ocamlfind ocamlc $(OPTIONS) $^ -a -o $@

rddl_client.cma: \
  rddl.cma \
  rddl_profiler.cmo \
  rddl_renderer.cmo \
  rddl_renderer_builtins.cmo
	ocamlfind ocamlc $(OPTIONS) -linkall $^ -a -o $@

%.cmo: %.ml
	ocamlfind ocamlc -g $(OPTIONS) -c $<

%.cmx: %.ml
	ocamlfind ocamlopt -g $(OPTIONS) -c $<

%.cmi: %.mli
	ocamlfind ocamlopt -g $(OPTIONS) -c $<

%.js: %.byte
	js_of_ocaml --pretty +weak.js $< -o $@

.depend: $(MLS) $(MLIS) Makefile
	ocamlfind ocamldep $(OPTIONS) $(MLS) $(MLIS) > $@

-include .depend

clean:
	-rm -f *.cm* rddl *~ *.o *.a *.dll *.dylib *.so
	-rm -f rddl-profiler-demo.js rddl-profiler-demo.byte rddl-checker
