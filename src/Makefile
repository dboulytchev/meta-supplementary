TOPFILE = Imp
OCAMLC = ocamlfind c
OCAMLOPT = ocamlfind opt
OCAMLDEP = ocamlfind dep
SOURCES = Imp.ml 
CAMLP5 = -syntax camlp5o -package ostap.syntax,GT.syntax,GT.syntax.all
PXFLAGS = $(CAMLP5)
BFLAGS = -rectypes -g -package GT,ostap
OFLAGS = $(BFLAGS) -inline 10

all:.depend $(TOPFILE)

.depend: $(SOURCES)
	$(OCAMLDEP) $(PXFLAGS) *.ml > .depend

$(TOPFILE): $(SOURCES:.ml=.cmx)
	$(OCAMLOPT) -o $(TOPFILE) $(OFLAGS) $(LIBS:.cma=.cmxa) -linkpkg $(SOURCES:.ml=.cmx) 

$(TOPFILE).byte: $(SOURCES:.ml=.cmo)
	$(OCAMLC) -o $(TOPFILE).byte $(BFLAGS) $(LIBS) -linkpkg $(SOURCES:.ml=.cmo) 

$(TOPFILE).js: $(TOPFILE).byte
	js_of_ocaml --source-map $<

clean:
	rm -Rf *.cmi *.cmo *.cmx *.annot *.o *.opt *.byte *~ .depend $(TOPFILE) $(TOPFILE).byte $(TOPFILE).js

-include .depend
# generic rules

###############
%.cmi: %.mli
	$(OCAMLC) -c $(BFLAGS) $(PXFLAGS) $<

# Note: cmi <- mli should go first
%.cmi: %.ml
	$(OCAMLC) -c $(BFLAGS) $(PXFLAGS) $<

%.cmo: %.ml
	$(OCAMLC) -c $(BFLAGS) $(PXFLAGS) $<

%.o: %.ml
	$(OCAMLOPT) -c $(OFLAGS) $(STATIC) $(PXFLAGS) $<

%.cmx: %.ml
	$(OCAMLOPT) -c $(OFLAGS) $(STATIC) $(PXFLAGS) $<

