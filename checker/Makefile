.PHONY: all

all: test typing

test: test.ml
	ocamlbuild test.byte && ./test.byte

typing: typing.ml
	ocamlbuild typing.byte
