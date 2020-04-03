.PHONY: all clean

all: test typing

test_all: test_utils test_z3utils test_typing

test_utils: utils.ml test_utils.ml
	ocamlbuild -use-ocamlfind test_utils.native \
	&& OCAMLRUNPARAM=b ./test_utils.native

test_z3utils: z3utils.ml test_z3utils.ml
	ocamlbuild -use-ocamlfind test_z3utils.native \
	&& OCAMLRUNPARAM=b ./test_z3utils.native

test_typing: typing.ml test_typing.ml
	ocamlbuild -use-ocamlfind test_typing.native \
	&& OCAMLRUNPARAM=b ./test_typing.native

clean:
	rm -rf *.byte *.native _build
