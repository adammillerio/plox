#!/usr/bin/env python3
from __future__ import annotations

from typing import TYPE_CHECKING, List

from lox.environment import Environment
from lox.lox_callable import LoxCallable
from lox.lox_return import Return
from lox.stmt import Function

if TYPE_CHECKING:
    from lox.interpreter import Interpreter
    from lox.lox_instance import LoxInstance


class LoxFunction(LoxCallable):
    """Lox function.

    Represents a Lox function, which is a collection of statements and a closure.

    The closure is a "stored" environment be used for things such as functions defined
    in scope of another function, to enclose/capture any variables referenced in
    the function which may be in the outer scope. For example in:
    fun makeCounter() {
      var i = 0;
      fun count() {
        i = i + 1;
        print i;
      }

      return count;
    }

    count is a closure which encloses i by storing it in this Environment

    Lox functions can be defined at any global or local scope, as well as on
    classes as methods.

    Args:
        declaration: Function. Function statement for this declaration, containing
            the body statements of the function.
        closure: Stored environment for closures.
        is_initializer: bool. If True, this method is the init() method for a Lox
            class and will be called when creating new instances.
    """

    def __init__(
        self, declaration: Function, closure: Environment, is_initializer: bool
    ) -> None:
        self.declaration = declaration
        self.closure = closure
        self.is_initializer = is_initializer

    def __repr__(self) -> str:
        # Override to custom string representation, to match Java and tests.
        return self.to_string()

    # Create a bound method on a class, creating an enclosing scope and
    # assigning a this keyword for instance access
    def bind(self, instance: "LoxInstance") -> LoxFunction:
        """Create a bound version of this method on a class instance.

        This creates an enclosing scope for the function with a "this" keyword
        registered which points to the instance which is invoking the method,
        similar to the self keyword in Python.

        Args:
            instance: LoxInstance. Lox class instance to bind this method to
                prior to calling.

        Returns:
            bound_method: LoxFunction. Copy of this funtion with the added this
                closure.
        """

        # Create the enclosing scope for this "bound" instance method on a
        # given class
        environment = Environment(self.closure)

        # Register "this" to correspond to the class instance which is
        # invoking this method
        environment.define("this", instance)

        # Return the LoxFunction method "bound" to the current instance
        return LoxFunction(self.declaration, environment, self.is_initializer)

    def to_string(self) -> str:
        # <fn add>
        return f"<fn {self.declaration.name.lexeme}>"

    def arity(self) -> int:
        """Return the arity of this function.

        Returns:
            arity: int. Number of arguments to the function.
        """

        return len(self.declaration.params)

    def call(self, interpreter: Interpreter, arguments: List[object]) -> object:
        """Call a Lox function.

        This sets up a new Environment in the scope chain and executes the
        statements in the function body.

        Args:
            interpreter: Interpreter. Runtime interpreter which is calling the
                function.
            arguments: List[object]. All arguments to the function, if any.
        """

        # Create a new environment in the scope chain for this function
        # This is done at call time and not declaration time, since the same
        # function can be called many times during recursion and each one needs
        # it's own scope. This Environment has the declaring scope's Environment
        # as a parent in order to access state from declaration scope at runtime
        environment = Environment(self.closure)

        # Bind all arguments to their named parameter values in the environment
        # fun add(a, b, c) { print a + b + c; }
        # add(1, 2, 3)
        # environment = { "a": 1, "b": 2, "c": 3 }
        for i in range(len(self.declaration.params)):
            environment.define(self.declaration.params[i].lexeme, arguments[i])

        # Execute function body using the constructed function scope environment
        # with mapped parameter values
        try:
            interpreter.execute_block(self.declaration.body, environment)
        except Return as return_value:
            # Return statement (exception) encountered in function, return to
            # the value to the calling scope, if any and end early
            # This is done with an exception in order to unwind any portions
            # of the stack inside the calling function (ifs/whiles/etc)

            # Constructor/initializer with an empty return is valid, but
            # should implicitly return "this"
            if self.is_initializer:
                return self.closure.get_at(0, "this")

            # Return the supplied value
            return return_value.value

        if self.is_initializer:
            # This is a constructor call, always return this
            # This is allowed even in subsequent explicit calls to init in
            # order to make the clox implementation simpler
            # var foo = Foo()
            # foo.init() <- Weird but valid, returns this
            return self.closure.get_at(0, "this")

        # No return statement in this function, so return nil implicitly
        return None
