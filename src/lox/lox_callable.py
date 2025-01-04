#!/usr/bin/env python3
from abc import ABCMeta, abstractmethod
from typing import TYPE_CHECKING, List

if TYPE_CHECKING:
    from lox.interpreter import Interpreter


class LoxCallable(metaclass=ABCMeta):
    @abstractmethod
    def arity(self) -> int: ...

    @abstractmethod
    def call(self, interpreter: "Interpreter", arguments: List[object]) -> object: ...
