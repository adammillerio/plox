#!/usr/bin/env python3
from sys import stderr
from typing import Union

from lox.runtime_error import LoxRuntimeError
from lox.token import Token
from lox.token_type import TokenType


class Lox:
    """Lox runtime control and utility class.

    This mirrors the Lox class in the jlox implementation, which contains the
    main method as well as methods for reporting errors to the user during all
    phases of interpreting a Lox source.

    The main() method itself has been broken out into the lox module's __init__.py
    which checks the error flags between stages of the interpreter. This "class"
    is just the collection of methods that control it.

    Public Attributes:
        had_error: bool. Whether or not an error was reported via the Lox.report()
            method while scanning or parsing the Lox source. If this is True,
            error(s) encountered will be reported to the user and the interpreter
            will not continue to the Resolver and Interpreter phases.
        had_runtime_error: bool. Whether or not an error was reported via the
            Lox.runtime_error() method while resolving the Lox source. If this
            is True, error(s) encountered will be reported to the user and the
            interpreter will not continue to the main Interpreter phase.
    """

    had_error = False
    had_runtime_error = False

    @classmethod
    def error(cls, line_or_token: Union[int, Token], message: str) -> None:
        """Report an error to the user.

        Args:
            line_or_token: Union[int, Token]. The line number (when scanning) or
                the Token (when parsing) that the error was encountered.
            message: str. Error message for the user, printed to stderr.

        Raises:
            RuntimeError: If a type other than int or Token is provided.
        """

        if isinstance(line_or_token, int):
            cls.error_line(line_or_token, message)
        elif isinstance(line_or_token, Token):
            cls.error_token(line_or_token, message)
        else:
            raise RuntimeError(
                f"Error must be line or token, got {type(line_or_token)}"
            )

    @classmethod
    def error_line(cls, line: int, message: str) -> None:
        """Report a line error to the user while scanning a Lox source.

        Args:
            line: int. Line number where the error was encountered while scanning.
            message: str. Error message for the user, printed to stderr.
        """

        cls.report(line, "", message)

    @classmethod
    def report(cls, line: int, where: str, message: str) -> None:
        """Report an error to the user while scanning or parsing.

        This is used by the Scanner and Parser to indicate errors encountered.
        These phases are not stopped when an error is encountered, instead a
        flag is set when this function is called which allows for continuing
        the current phase in order to report subsequent errors to the user.

        Args:
            line: int. Line number where the error was encountered while scanning.
            where: str. String representation of where on the line the error occurred.
            message: str. Error message for the user, printed to stderr.
        """

        print(f"[line {line}] Error{where}: {message}", file=stderr)
        cls.had_error = True

    @classmethod
    def error_token(cls, token: Token, message: str) -> None:
        """Report a Token error to the user while parsing a Lox source.

        Args:
            token: Token. Token where the error was encountered.
            message: str. Error message for the user, printed to stderr.
        """

        if token.type is TokenType.EOF:
            cls.report(token.line, " at end", message)
        else:
            cls.report(token.line, f" at '{token.lexeme}'", message)

    @classmethod
    def runtime_error(cls, error: LoxRuntimeError) -> None:
        """Report a runtime error to the user while resolving a Lox source.

        Similar to the Scanner/Parser, errors during the Resolver do not immediately
        stop execution, in order to allow for subsequent resolution errors to be
        indicated in the source.

        During the Interpreter phase, LoxRuntimeErrors are raised directly in
        order to halt execution.

        Args:
            error: LoxRuntimeError. Runtime error to report to the user.
        """

        print(f"{error}\n[line {error.token.line}]", file=stderr)
        cls.had_runtime_error = True
