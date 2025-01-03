#!/usr/bin/env python3
from dataclasses import dataclass

from lox.token_type import TokenType


@dataclass(frozen=True)
class Token:
    type: TokenType
    lexeme: str
    literal: object
    line: int

    def __repr__(self) -> str:
        return self.to_string()

    def to_string(self) -> str:
        return str(self.type) + " " + self.lexeme + " " + str(self.literal)
