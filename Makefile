OCAMLC=ocamlfind ocamlc
OCAMLC=ocamlfind ocamlopt

.PHONY: doc clean

all: ligHTML.cmo ligHTML.cmx doc

ligHTML.cmo: ligHTML.cmi
ligHTML.cmx: ligHTML.cmi
doc: ligHTML.cmo

%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmx: %.ml
	$(OCAMLOPT) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<

doc:
	-mkdir -p doc
	ocamldoc -html -d doc ligHTML.mli

clean:
	-rm -rf doc *.cm* *~ *.o
