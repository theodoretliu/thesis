.PHONY: all clean

all: test typing

test: test_typing.ml test_z3utils.ml
	ocamlbuild -use-ocamlfind test_typing.native \
	&& ocamlbuild -use-ocamlfind test_z3utils.native \
	&& ./test_typing.native \
	&& ./test_z3utils.native

typing: typing.ml
	ocamlbuild typing.byte

clean:
	rm -rf *.byte *.native _build
