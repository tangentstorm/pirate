# pirate — Python on Parrot

**pirate** is a Python compiler targeting the [Parrot](https://en.wikipedia.org/wiki/Parrot_virtual_machine)
virtual machine — the register-based VM originally built to run Perl 6.
Given a Python source file, pirate compiles it to Parrot IMC (intermediate code),
which the Parrot assembler then turns into bytecode the VM can execute.

```
python pirate.py [-d] [-t] filename.py
```

This is a historical project, active roughly 2003–2008. It is preserved here
as an archive of early "language X on Parrot" work, not as a maintained tool.

## Historical context

In the early 2000s, Parrot was conceived as a common runtime for many dynamic
languages — Perl 6 first, but also Python, Ruby, PHP, Scheme, and others.
A wave of ports followed: Perl 5 via Ponie, Ruby, Lua, and others.

pirate was Michal Wallace's Python entry in that wave (started August 2003,
version 0.01 alpha). It began life as an extension of Andrew Kuchling's
`parrot-gen.py` experiment and grew into a fuller compiler. The approach:

1. Parse Python source with CPython's own `compiler` package (Python 2.2+ AST).
2. Walk the AST and emit Parrot IMC instructions (`pirate.py`, `transform.py`, `simple.py`).
3. Assemble and run the IMC on the Parrot VM, using Python-flavored
   PMCs (Parrot's object model) for runtime behavior (`pirate-pmc/`).

## Status and completeness

At its peak, pirate compiled and ran a meaningful subset of Python 2:

- ints, floats, strings, lists, tuples, dicts
- assignment and multi-assignment (`x, y = 1, 2`)
- `while`/`else`, `for`/`else`, `if`/`elif`/`else`, `break`, `continue`
- `try`/`except`, `raise`, `assert`
- math, boolean logic, and comparison operators
- functions and calls (including the Parrot calling convention), lambdas
- list comprehensions, generators, proper lexical scoping

Still missing (per the original to-do list): **classes** (Parrot's object model
wasn't ready), **`import`/`exec`/`eval`**, built-in functions, and the standard
library. Generated code was completely unoptimized — the microthreads demo ran
roughly 8–9× slower than CPython.

There are 50+ small test cases under `pirate/test/python/` covering much of the
supported language, plus a test-first runner (`pirate/PirateTest.py`).

## Repo layout

| Path | What it is |
|---|---|
| `pirate/pirate.py` | Main compiler driver: AST → Parrot IMC |
| `pirate/transform.py`, `pirate/simple.py`, `pirate/phase1.py` | AST transforms and code-generation helpers |
| `pirate/PirateTest.py` | Test-first runner (`python PirateTest.py`) |
| `pirate/test/python/` | 50+ Python test programs for the compiler |
| `pirate/test/pirate/` | Parrot-side test fixtures |
| `pirate/toys/` | Experiments (parsers, transforms, codegen sketches) |
| `pirate/tools/` | Utilities (e.g. `traceparrot.py`) |
| `pirate/benchmarks/` | Benchmark programs |
| `pirate-pmc/` | C-level Parrot PMCs implementing Python runtime behavior, with `t/` tests |
| `pirate/python/` | Python-side runtime support files |
| `ecdysis/` | A separate, unrelated language experiment by the same author |
| `parrakeet/` | PHP grammar/parser experiments (dparser-based) |
| `site/` | The original project website (circa 2003) |
| `CVSROOT/` | Leftover from the original CVS import |

The original design documents survive as plain-text files in `pirate/`:
`README` (status and to-do), `GOALS` (goals for pirate and "dutchman"),
`GUIDE` (installation and code tour), `PROGRESS` (compiler-method checklist),
`CREDITS`, and `INTEROP`.

## Building (historical)

These steps are preserved from the original `GUIDE`; they describe a
2003-era toolchain (Parrot from Subversion trunk, Python 2.2/2.3 built with
`--enable-shared`) and will not work unmodified on modern systems.

1. Check out and build Parrot:
   ```
   svn co https://svn.perl.org/parrot/trunk parrot
   cd parrot
   perl Configure.pl --prefix=`pwd`
   make
   ```
   (`--prefix` must point at the Parrot tree so it can find the Python PMCs.)
2. Build Python 2.2+ as a shared library: `./configure --enable-shared && make`
3. Run the test suite: `python PirateTest.py`
   (`dump=1` prints generated IMC; `lines=0` hides line numbers for piping to `.imc` files.)

## See also

- [Parrot virtual machine](https://en.wikipedia.org/wiki/Parrot_virtual_machine) — the VM this project targeted
- [tangentproofs](https://github.com/tangentproofs/) — the author's later formalization work
