#!/usr/bin/env python3
from abc import ABCMeta, abstractmethod
from typing import TYPE_CHECKING, List

if TYPE_CHECKING:
    from lox.interpreter import Interpreter


class LoxCallable(metaclass=ABCMeta):
    """Abstract base class for a Lox callable.

    In Lox, both Functions and Classes are callable:
    clock(); <- Returns the current time.
    Foo();   <- Returns a new instance of the Foo class.

    Callables must implement arity() for determining the number of arguments,
    and call() for executing the call action.
    """

    @abstractmethod
    def arity(self) -> int: ...

    @abstractmethod
    def call(self, interpreter: "Interpreter", arguments: List[object]) -> object: ...
