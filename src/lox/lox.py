#!/usr/bin/env python3
from sys import stderr
from typing import Union

from lox.runtime_error import LoxRuntimeError
from lox.token import Token
from lox.token_type import TokenType


class Lox:
    had_error = False
    had_runtime_error = False

    @classmethod
    def error(cls, line_or_token: Union[int, Token], message: str) -> None:
        if isinstance(line_or_token, int):
            cls.error_line(line_or_token, message)
        elif isinstance(line_or_token, Token):
            cls.error_token(line_or_token, message)
        else:
            raise RuntimeError(
                f"Error must be line or token, got {type(line_or_token)}"
            )
        pass

    @classmethod
    def error_line(cls, line: int, message: str) -> None:
        cls.report(line, "", message)

    @classmethod
    def report(cls, line: int, where: str, message: str) -> None:
        print(f"[line {line}] Error{where}: {message}", file=stderr)
        cls.had_error = True

    @classmethod
    def error_token(cls, token: Token, message: str) -> None:
        if token.type is TokenType.EOF:
            cls.report(token.line, " at end", message)
        else:
            cls.report(token.line, f" at '{token.lexeme}'", message)

    @classmethod
    def runtime_error(cls, error: LoxRuntimeError) -> None:
        print(f"{error}\n[line {error.token.line}]", file=stderr)
        cls.had_runtime_error = True
