#!/usr/bin/env python3
from typing import TYPE_CHECKING, Dict

from lox.runtime_error import LoxRuntimeError
from lox.token import Token

if TYPE_CHECKING:
    from lox.lox_class import LoxClass


class LoxInstance:
    """An instance of a Lox class.

    When a LoxClass is called, it will instantiate an instance of the class, which
    has it's own set of fields and a reference to the LoxClass itself. All method
    calls will use the bind() method on LoxFunction in order to provide a version
    of the method which has a "this" keyword referencing the instance itself.

    Args:
        klass: LoxClass. Lox class definition that this is an instance of.

    Public Attributes:
        fields: Dict[str, object]. Mapping of field names to their set values for
            this instance.
    """

    def __init__(self, klass: "LoxClass") -> None:
        self.klass = klass
        self.fields: Dict[str, object] = {}

    def __repr__(self) -> str:
        # Override to custom string representation, to match Java and tests.
        return self.to_string()

    def get(self, name: Token) -> object:
        """Get a field from this instance.

        A field is either a property ie foo.bar = true or a bound method ie bar()

        Args:
            name: Token. Token with the name of the field to access.

        Returns:
            property_or_method: object. Either a LoxFunction representing a method
                bound to this instance, or the value of a property for this instance.

        Raises:
            LoxRuntimeError: If there is no property with this name on the instance,
                and no method with this name on the LoxClass.
        """

        if name.lexeme in self.fields:
            return self.fields[name.lexeme]

        method = self.klass.find_method(name.lexeme)
        if method is not None:
            return method.bind(self)

        raise LoxRuntimeError(name, f"Undefined property '{name.lexeme}'.")

    def set(self, name: Token, value: object) -> None:
        """Set a property on this instance.

        A property will shadow any method on the LoxClass with the same name.

        Args:
            name: Token. Token with the name of the field to set.
            value: object. Python object representing the runtime value to set
                for this field.
        """

        self.fields[name.lexeme] = value

    def to_string(self) -> str:
        # Foo instance
        return f"{self.klass.name} instance"
