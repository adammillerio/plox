#!/usr/bin/env python3
from pathlib import Path
from sys import argv, exit, stderr

from lox.ast_printer import AstPrinter
from lox.interpreter import Interpreter
from lox.lox import Lox
from lox.parser import Parser
from lox.resolver import Resolver
from lox.scanner import Scanner


def main() -> None:
    # First argument in argv is always the script itself in Python
    if len(argv) == 2:
        run_file(argv[1])
    elif len(argv) == 3:
        command = argv[1]
        match command:
            case "run":
                source = argv[2]
                run(source)
            case "run_file":
                file = argv[2]
                run_file(file)
            case "run_prompt":
                run_prompt()
            case "print_ast":
                file = argv[2]
                print_ast(file)
            case _:
                print(f"unrecognized command: {command}", file=stderr)
                exit(66)

    elif len(argv) > 3:
        print("Usage: plox [command] [script]")
        exit(64)
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


def print_ast(path: str) -> None:
    # Additional method for printing the full AST from statements, using the
    # reference full AST printer from munificent/craftinginterpreters. The book
    # covers the AST printer early on but the full version is only in the example
    # repo.
    script_path = Path(path)
    if not script_path.exists():
        print(f"File at {script_path} not found", file=stderr)
        exit(66)

    script = Path(path).read_text()

    scanner = Scanner(script)
    tokens = scanner.scan_tokens()
    parser = Parser(tokens)
    statements = parser.parse()

    # Stop if there was a syntax error
    if Lox.had_error:
        return

    printer = AstPrinter()
    for i, statement in enumerate(statements, 1):
        print(f"{i}: {printer.print(statement)}")


if __name__ == "__main__":
    main()
