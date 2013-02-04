main:
	ocamlbuild -use-ocamlfind -pkg zarith -tag annot -ocamlyacc "ocamlyacc -v" src/main.byte
