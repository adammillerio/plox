#!/usr/bin/env python3
from __future__ import annotations

from typing import TYPE_CHECKING, Dict, List, Optional

from lox.lox_callable import LoxCallable
from lox.lox_function import LoxFunction
from lox.lox_instance import LoxInstance

if TYPE_CHECKING:
    from lox.interpreter import Interpreter


class LoxClass(LoxCallable):
    """Lox Class.

    Represents the Python implementation of a Lox class.

    For example:
    class Doughnut {
      cook() {
        print "Fry until golden brown.";
      }
    }

    class BostonCream < Doughnut {
        init() {
            print "I am a delicious donut";
        }
    }

    Will define a class Doughnut with no superclass and one method, cook, as well
    as a BostonCream, which has Doughnut as a superclass, and an initializer method.

    Because LoxClass implements LoxCallable, it can be called to instantiate a
    new LoxInstance of the class, where methods can be bound and called:
    BostonCream().cook()
    I am a delicious donut
    Fry until golden brown.

    Args:
        name: str. Name of the class.
        superclass: Optional[LoxClass]. Superclass of this class, if any.

    Public Attributes:
        methods: Dict[str, LoxFunction]. Mapping of method name to LoxFunction
            for all methods defined on this class.
    """

    def __init__(
        self,
        name: str,
        superclass: Optional[LoxClass] = None,
        methods: Optional[Dict[str, LoxFunction]] = None,
    ) -> None:
        self.name = name
        self.superclass = superclass
        self.methods: Dict[str, LoxFunction] = methods if methods else {}

    def __repr__(self) -> str:
        # Override to custom string representation, to match Java and tests.
        return self.to_string()

    def find_method(self, name: str) -> Optional[LoxFunction]:
        """Retrieve a method on this class by name.

        Args:
            name: str. Name of the class method.

        Returns:
            method: Optional[LoxFunction]. Function definition if found.
        """

        if name in self.methods:
            return self.methods[name]

        # Check if the method exists on the superclass if one is defined
        if self.superclass is not None:
            return self.superclass.find_method(name)

        return None

    def to_string(self) -> str:
        # Foo
        return self.name

    def call(self, interpreter: "Interpreter", arguments: List[object]) -> LoxInstance:
        """Call a Lox class.

        Calling a Lox class will create a new instance of the class, running the
        init() constructor method if one is defined.

        Args:
            interpreter: Interpreter. Runtime interpreter which is calling the class.
            arguments: List[object]. All arguments to the init() method, if any.

        Returns:
            instance: LoxInstance. New instance of this class.
        """

        instance = LoxInstance(self)

        # Look up the constructor method if any, bind it, and execute it
        initializer = self.find_method("init")
        if initializer is not None:
            initializer.bind(instance).call(interpreter, arguments)

        return instance

    def arity(self) -> int:
        """Get the arity of a Lox class.

        Returns:
            arity: int. Number of arguments to the init() method if one is defined,
                or 0 otherwise.
        """

        initializer = self.find_method("init")
        # No constructor method
        if initializer is None:
            return 0

        # Return the arity of the constructor method
        return initializer.arity()
