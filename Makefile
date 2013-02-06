PACKAGES := -pkgs zarith,num
OCAMLBUILD := ocamlbuild -use-ocamlfind $(PACKAGES) -tag annot \
	                 -ocamlyacc "ocamlyacc -v"

main: main.byte

main.byte:
	$(OCAMLBUILD) src/main.byte

main.native:
	$(OCAMLBUILD) src/main.native
