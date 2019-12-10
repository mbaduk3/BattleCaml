MODULES=gameboard gameloop authors display command ai_medium ai_hard ai_easy
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
MAIN=gameloop
OCB_FLAGS = -tag bin_annot
OCAMLBUILD=ocamlbuild -use-ocamlfind $(OCB_FLAGS)

native:
	$(OCAMLBUILD) $(MAIN).native

byte:
	$(OCAMLBUILD) $(MAIN).byte

default: build

build:
	$(OCAMLBUILD) $(OBJECTS)

zip: build
	zip battlecaml_src.zip *.ml* _tags Makefile INSTALL.md

play: native 
	./$(MAIN).native

clean:
	ocamlbuild -clean
