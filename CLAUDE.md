# CLAUDE.md

## Building the checker

The OCaml shape checker lives in `checker/` and builds with the local opam
switch at `checker/_opam` (dune 3.24). Run dune from `checker/`:

```sh
dune build
dune test --force
dune fmt
```

A SessionStart hook in `.claude/settings.json` writes
`opam env --switch=checker --set-switch` into the session environment, so plain
`dune` resolves to the local switch. Without it, `dune` falls through to the
global default switch (`coq818`, dune 3.22), which fails with "Version 3.24 of
the dune language is not supported".

If you see that error, the hook didn't apply (e.g. the local switch doesn't
exist yet). Either create the switch per `checker/README.md`, or prefix
commands explicitly:

```sh
opam exec --switch=. -- dune build
```

## The Python frontend

`frontend/` is the jaxtyping frontend (Python 3.9+, stdlib only; see
`frontend/README.md`). It shells out to the checker CLI at
`checker/_build/default/bin/shapecheck.exe`, so build the checker first. From
`frontend/`:

```sh
python -m shapecheck ../examples/pass/*.py
python -m unittest discover -s tests -t .
uvx ruff format . && uvx ruff check .
```

`examples/pass/*.py` must check, and each `examples/fail/*.py` must fail with
every `# expect-error:` substring in its output.
