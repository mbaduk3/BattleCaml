MODULES=gameboard gameloop authors
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
	zip battlecaml_src.zip *.ml* _tags Makefile report/*

play: native 
	./$(MAIN).native

clean:
	ocamlbuild -clean
