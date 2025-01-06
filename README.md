# plox

A pure python implementation of the lox language from crafting interpreters.

This implementation has full type annotation coverage and checking using the
[pyre](https://github.com/facebook/pyre-check) type checker in strict mode. It
also follows the Java implementation's general structure as much as possible, with
a few small changes. It has no other dependencies.

Lox was created by Robert Nystrom as part of the [Crafting Interpreters](https://craftinginterpreters.com/)
book.

The sample repo has a comprehensive test suite which `plox` passes as well. Refer
to the testing section of the README for more info.

# Usage

The quickest way to start using plox is with the [uv](https://github.com/astral-sh/uv)
package manager:

```bash
# See uv README for other installation options
curl -LsSf https://astral.sh/uv/install.sh | sh
```

Then, use the packaged version of the CLI to start the REPL or run a file:

```bash
alias plox='uvx --no-dev --from plox-lang plox'
plox examples/sayHi.lox
Hi, Dear Reader!
```

Or, with a locally cloned version:

```bash
git clone https://github.com/adammillerio/plox.git
cd plox
uv run --no-dev plox examples/sayHi.lox
Hi, Dear Reader!
```

There is also a `plox.sh` script which can be used to invoke `plox` from anywhere.

The CLI functions exactly like the reference `jlox` implementation found in the
author's [example code repo](https://github.com/munificent/craftinginterpreters).

There is one additional CLI function to print the AST:
```bash
uv run --no-dev plox print_ast examples/sayHi.lox
1: (fun sayHi(first last) (print (+ (+ (+ (+ Hi,  first)  ) last) !)))
2: (; (call sayHi  Dear Reader))
```

# Testing

[Original Docs](https://github.com/munificent/craftinginterpreters?tab=readme-ov-file#testing)

The sample repo includes a test suite of lox files for validation. It uses the
dart language, which can be installed via Homebrew on macOS:

```bash
brew install dart-lang/dart/dart@2.19
# brew link will make this the "primary" version of dart on your machine
# Alternatively, it can be added to the PATH for the current session:
# export PATH="$(brew --prefix)/opt/dart@2.19/bin:$PATH"
brew link dart-lang/dart/dart@2.19
```

Additionally, `clox` is built as a dependency of the test suite, so a C compiler
and `make` are required.

The `test.sh` script will clone a copy of the sample repo, build `clox` and the
dart test snapshot, then run the full `jlox` test suite:

```bash
./test.sh
All 239 tests passed (556 expectations).
```

# Development

All development on plox can be handled through the `uv` tool:

```bash
uv sync
Resolved 20 packages in 0.46ms
Installed 18 packages in 175ms
```

Invocations of `uv` will read configuration from the [pyproject.toml](pyproject.toml)
file and configure a virtual environment with `plox` and it's dependencies under
`.venv` in the repository.

## Type Checking

Ensure no type errors are present with [pyre](https://github.com/facebook/pyre-check):

```bash
uv run pyre check
ƛ No type errors found
```

**Note**: Pyre daemonizes itself on first run for faster subsequent executions. Be
sure to shut it down with `uv run pyre stop` when finished.

## Formatting

Format code with the [ruff](https://github.com/astral-sh/ruff) formatter:

```bash
uv run ruff format
17 files left unchanged
```

