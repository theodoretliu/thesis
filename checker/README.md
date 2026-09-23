# numpy_checking

Z3-backed shape checker for numpy array programs.

## Setup

Requires [opam](https://opam.ocaml.org) 2.2+ with its shell hook enabled
(`opam init --enable-shell-hook`), so the local switch below is picked up
automatically whenever you `cd` into this directory.

```sh
cd checker
opam switch create . --empty
# conf-python-3 asks Homebrew for python@3.9, but any python3 works.
opam option 'depext-bypass=["python@3.9"]'
opam install . --deps-only --locked --with-test --with-dev-setup
```

On macOS, opam will offer to `brew install llvm@17`, which the z3 package
requires. Building z3 from source takes several minutes.

## Development

```sh
dune build
dune test
dune test --watch
dune fmt
```

## Updating dependencies

Dependencies are declared in `dune-project`; `numpy_checking.opam` is
generated from it by `dune build`. After changing them, refresh the lock
file with:

```sh
opam install . --deps-only --with-test --with-dev-setup
opam lock .
```
