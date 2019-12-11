MODULES=gameboard gameloop authors display command ai_medium ai_hard ai_easy ascii rules
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=gameloop
OCB_FLAGS = -tag bin_annot
OCAMLBUILD=ocamlbuild -use-ocamlfind $(OCB_FLAGS)

native:
	$(OCAMLBUILD) $(MAIN).native

byte:
	$(OCAMLBUILD) $(MAIN).byte

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

default: build

build:
	$(OCAMLBUILD) $(OBJECTS)

zip: build
	zip battlecaml_src.zip *.ml* _tags Makefile INSTALL.md

play: native 
	./$(MAIN).native

clean:
	ocamlbuild -clean
