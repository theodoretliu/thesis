"""python -m shapecheck [--dump-ir] FILE...: check jaxtyping-annotated Python
files with the OCaml shape checker."""

from __future__ import annotations

import argparse
import json
import os
import re
import subprocess
import sys
from collections.abc import Sequence
from dataclasses import dataclass
from pathlib import Path

from . import ir
from .signatures import FrontendError
from .translate import Stubs, load_stubs, translate

FRONTEND = Path(__file__).resolve().parent.parent
DEFAULT_STUBS = FRONTEND / "stubs"
DEFAULT_CHECKER = FRONTEND.parent / "checker" / "_build" / "default" / "bin" / "shapecheck.exe"


@dataclass
class Report:
    errors: list[str]  # "path:line: message"
    notes: list[str]
    checked: int  # functions whose bodies went to the checker
    passed: list[str]  # functions whose bodies checked without errors


def checker_path(explicit: str | None = None) -> Path:
    path = Path(explicit or os.environ.get("SHAPECHECK_CHECKER") or DEFAULT_CHECKER)
    if not path.exists():
        raise FrontendError(
            f"the checker isn't built at {path}; run `dune build` in checker/ "
            "(or set SHAPECHECK_CHECKER)"
        )
    return path


def run_checker(program: ir.Json, checker: Path) -> list[dict]:
    proc = subprocess.run([str(checker)], input=json.dumps(program), capture_output=True, text=True)
    if proc.returncode not in (0, 1):
        raise FrontendError(f"the checker failed: {proc.stderr.strip()}")
    return json.loads(proc.stdout)["results"]


# the checker reports a statement as "in f, line 7, `z = x @ w`: ..."
LOCATED = re.compile(r"in (\S+), line (\d+), (.*)", re.S)


def check_file(path: Path, stubs: Stubs, checker: Path) -> Report:
    source = path.read_text()
    program, t = translate(source, stubs, str(path))
    errors = [
        (e.line, f"in {e.function}: {e.message}" if e.function else e.message) for e in t.errors
    ]
    lines = {f.node.name: f.node.lineno for f in t.functions.values()}
    results = run_checker(program, checker) if program["functions"] else []
    for r in results:
        if r["error"] is None:
            continue
        m = LOCATED.match(r["error"])
        if m:
            errors.append((int(m.group(2)), f"in {m.group(1)}, {m.group(3)}"))
        else:
            errors.append((lines.get(r["name"], 1), r["error"]))
    checked = sum(f["body"] is not None for f in program["functions"])
    failed = {r["name"] for r in results if r["error"] is not None}
    passed = [
        f["name"] for f in program["functions"]
        if f["body"] is not None and f["name"] not in failed
    ]  # fmt: skip
    errors.sort(key=lambda e: e[0])
    return Report(
        errors=[f"{path}:{line}: {msg}" for line, msg in errors],
        notes=[f"{path}:{n.line}: note: {n.message}" for n in t.notes],
        checked=checked,
        passed=passed,
    )


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(prog="shapecheck", description=__doc__)
    parser.add_argument("files", nargs="+", type=Path)
    parser.add_argument("--dump-ir", action="store_true", help="print the IR instead of checking")
    parser.add_argument(
        "--stubs", action="append", type=Path, default=[], help="another stub directory"
    )
    parser.add_argument("--checker", help="the shapecheck executable (default: checker/_build)")
    args = parser.parse_args(argv)

    try:
        stubs = load_stubs([DEFAULT_STUBS] + args.stubs)
        if args.dump_ir:
            failed = False
            for path in args.files:
                program, t = translate(path.read_text(), stubs, str(path))
                print(ir.dumps(program))
                for e in t.errors:
                    failed = True
                    where = f"in {e.function}: " if e.function else ""
                    print(f"{path}:{e.line}: {where}{e.message}", file=sys.stderr)
            return 1 if failed else 0
        checker = checker_path(args.checker)
        failed = False
        for path in args.files:
            report = check_file(path, stubs, checker)
            for line in report.notes + report.errors:
                print(line)
            if report.errors:
                failed = True
            else:
                n = report.checked
                print(f"{path}: ok ({n} function{'' if n == 1 else 's'} checked)")
        return 1 if failed else 0
    except FrontendError as e:
        print(f"shapecheck: {e.message}", file=sys.stderr)
        return 2
