# plox

A python implementation of the lox language from crafting interpreters.

This implementation has full type annotation coverage and checking using the
[pyre](https://github.com/facebook/pyre-check) type checker in strict mode. It
also follows the Java implementation's general structure as much as possible, with
a few small changes.

Lox was created by Robert Nystrom as part of the [Crafting Interpreters](https://craftinginterpreters.com/)
book.

# Usage

The quickest way to start using plox is with the [uv](https://github.com/astral-sh/uv)
package manager:

```bash
# See uv README for other installation options
curl -LsSf https://astral.sh/uv/install.sh | sh
```

Then, use the packaged version of the CLI to start the REPL or run a file:

```bash
alias plox='uvx --from plox plox'
plox examples/sayHi.lox
Hi, Dear Reader!
```

Or, with a locally cloned version:

```bash
git clone https://github.com/adammillerio/plox.git
cd plox
uv run plox examples/sayHi.lox
Hi, Dear Reader!
```

The CLI functions exactly like the reference `jlox` implementation found in the
author's [example code repo](https://github.com/munificent/craftinginterpreters).

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

