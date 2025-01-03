#!/usr/bin/env python3
from pathlib import Path
from sys import argv, exit, stderr

from lox.interpreter import Interpreter
from lox.lox import Lox
from lox.parser import Parser
from lox.resolver import Resolver
from lox.scanner import Scanner


def main() -> None:
    # First argument in argv is always the script itself in Python
    if len(argv) > 2:
        print("Usage: plox [script]")
        exit(64)
    elif len(argv) == 2:
        run_file(argv[1])
    else:
        run_prompt()


def run_file(path: str) -> None:
    script_path = Path(path)
    if not script_path.exists():
        print(f"File at {script_path} not found", file=stderr)
        exit(66)

    script = Path(path).read_text()

    run(script)

    # Indicate an error in the exit code.
    if Lox.had_error:
        exit(65)
    elif Lox.had_error:
        exit(70)


def run_prompt() -> None:
    try:
        while True:
            line = input("> ")

            if not line:
                break

            run(line)

            # Reset error flag since this is an interactive session.
            Lox.had_error = False
    except KeyboardInterrupt:
        return


def run(source: str) -> None:
    interpreter = Interpreter()

    scanner = Scanner(source)
    tokens = scanner.scan_tokens()
    parser = Parser(tokens)
    statements = parser.parse()

    # Stop if there was a syntax error
    if Lox.had_error:
        return

    resolver = Resolver(interpreter)
    resolver.resolve(statements)

    # Stop if there was a resolution error.
    if Lox.had_error:
        return

    interpreter.interpret(statements)


if __name__ == "__main__":
    main()
