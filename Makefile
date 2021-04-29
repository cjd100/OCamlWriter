MODULES=gui file author customize words
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte 
MAIN=gui.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -r -tag thread

default: build 
	OCAMLRUNPARAM=b utop

# TEMPORARY: Displays the GUI, but there's probably a better way to do it
#
# If you are on windows, you need xming or another x-server program open for 
# this to work
gui_temp:
	ocamlfind ocamlc -g -package lablgtk2 -linkpkg gui.mli -o gui
	ocamlfind ocamlc -g -package lablgtk2 -linkpkg gui.ml -o gui 
	./gui
	rm gui.cmi
	rm gui.cmo
	rm gui

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

run:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

docs: docs-public docs-private
	
docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package core,lablgtk2 \
		-html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package core,lablgtk2 \
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

zip:
	zip text_editor.zip *.ml* *.json *.sh *.md *.txt _tags .merlin .ocamlformat .ocamlinit Makefile	

cloc:
	cloc --by-file --include-lang=OCaml --exclude-dir=_build .
