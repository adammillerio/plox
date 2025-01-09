#!/usr/bin/env python3
from lox.token import Token


class LoxRuntimeError(RuntimeError):
    """Exception representing an error that was encountered in the Lox interpreter.

    Runtime errors immediately interrupt execution and are reported to stderr.

    Args:
        token: Token. Token where the error was encountered.
        message: str. Error message with details.
    """

    def __init__(self, token: Token, message: str) -> None:
        super().__init__(message)
        self.token = token
