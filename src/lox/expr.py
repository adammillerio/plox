#!/usr/bin/env python3
from __future__ import annotations

from abc import ABCMeta, abstractmethod
from dataclasses import dataclass
from typing import Generic, List, TypeVar

from lox.token import Token

# TypeVariable for the return type of the Visitor interface. TypeVars are invariant
# by default, which means that a class implementing Visitor[str] must return str,
# and not any subtype of it. This isset to covariant to allow for Visitor[object]
# as it is in the Java implementation.
# For more info:
# https://peps.python.org/pep-0484/#covariance-and-contravariance
R = TypeVar("R", covariant=True)


# eq=False ensures the default behavior in Python's (and Java's) object classes
# is used for equality and hashing, which is to use the object's unique identifier
# in memory using the id() function. This avoids exprs/stmts at different scopes
# having the same hash.
@dataclass(eq=False, frozen=True)
class Expr(metaclass=ABCMeta):
    """Base class for a Lox expression.

    An expression is a sequence of Tokens which evaluates to a value.

    For example:
    tokens = Scanner("2;").scan_tokens()
    statements = Parser(tokens).parse()

    Produces a list with a single expression statement of the simplest type:
    Expression(expression=Literal(2.0))

    Expressions in Lox are implemented using the visitor pattern. As such, all
    expressions extend a single method, accept, which will route the expression
    to the correct visitor method on the invoking instance.
    """

    @abstractmethod
    def accept(self, visitor: Visitor[R]) -> R: ...


@dataclass(eq=False, frozen=True)
class Assign(Expr):
    """Assignment expression.

    Represents the assignment of a Lox variable to a value. It evaluates to the
    value being assigned.

    For example:
    tokens = Scanner("a = 2;").scan_tokens()
    statements = Parser(tokens).parse()
    print(statements[0])

    Expression(
        expression=Assign(
            name=Token(TokenType.IDENTIFIER, "a", None, 1),
            value=Literal(2.0),
        ),
    )

    Args:
        name: Token. Name of the assignment target.
        value: Expr. Expression to evaluate for value.
    """

    name: Token
    value: Expr

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_assign_expr(self)


@dataclass(eq=False, frozen=True)
class Binary(Expr):
    """Binary expression.

    Comparison operators, which evaluate to a bool:
    > >= < <=

    Equality operators, which evaluate to a bool:
    != ==

    Arithmetic operators, which evaluate to a float:
    - + / *

    The + operator can also evaluate to a string for concatenation.

    For example:
    tokens = Scanner("2 + 2;").scan_tokens()
    statements = Parser(tokens).parse()
    print(statements[0])

    Expression(
        expression=Binary(
            left=Literal(2.0),
            right=Literal(2.0),
            operator=Token(TokenType.PLUS, "+", None, 1),
        ),
    )

    Args:
        left: Expr. Expression to evaluate for left operand.
        operator. Token. Token representing the binary operation to perform.
        right. Expr. Expression to evaluate for right operand.
    """

    left: Expr
    operator: Token
    right: Expr

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_binary_expr(self)


@dataclass(eq=False, frozen=True)
class Call(Expr):
    """Call expression.

    Represents the invocation of a function or a method on a class, which will
    evaluate to a "bound" method on the Lox instance being accessed.

    For example:
    tokens = Scanner("foo(bar);").scan_tokens()
    statements = Parser(tokens).parse()
    print(statements[0])

    Expression(
        expression=Call(
            callee=Variable(Token(TokenType.IDENTIFIER, "foo", None, 1)),
            paren=Token(TokenType.RIGHT_PAREN, ")", None, 1),
            arguments=[Variable(Token(TokenType.IDENTIFIER, "bar", None, 1))],
        ),
    )

    Args:
        callee: Expr. Expression to evaluate for the callee, this is an Expr
            rather than a Variable to allow for chaining of multiple calls ie
            foo.bar().baz()
        paren: Token. Token representing the right parenthesis of the call.
        arguments: List[Expr]. All expressions to evaluate for arguments to the
            call, if any.
    """

    callee: Expr
    paren: Token
    arguments: List[Expr]

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_call_expr(self)


@dataclass(eq=False, frozen=True)
class Get(Expr):
    """Get expression.

    Represents the retrieval of a property from a given Lox class instance, and
    evaluates to the retrieved value.

    For example:
    tokens = Scanner("foo.bar;").scan_tokens()
    statements = Parser(tokens).parse()
    print(statements[0])

    Expression(
        expression=Get(
            object=Variable(Token(TokenType.IDENTIFIER, "foo", None, 1)),
            name=Token(TokenType.IDENTIFIER, "bar", None, 1)),
        ),
    )

    Args:
        object: Expr. Expression to evaluate for the object to access properties
            on, this is an Expr rather than a Variable to allow for the chaining
            of multiple gets ie foo.bar.baz
        name: Token. Token representing the property to access on the object.
    """

    object: Expr
    name: Token

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_get_expr(self)


@dataclass(eq=False, frozen=True)
class Grouping(Expr):
    """Grouping expression.

    Represents any expression grouped by parenthesis for scoping, evaluates to
    the result of the inner expression.

    For example:
    tokens = Scanner("(2);").scan_tokens()
    statements = Parser(tokens).parse()
    print(statements[0])

    Expression(expression=Grouping(Literal(2.0)))

    Args:
        expression: Expr. Inner expression of the grouping.
    """

    expression: Expr

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_grouping_expr(self)


@dataclass(eq=False, frozen=True)
class Literal(Expr):
    """Literal expression.

    Represents and evaluates to a literal value within Lox, containing the Python
    equivalent representation of it:
    false -> Literal(False)
    true  -> Literal(True)
    nil   -> Literal(None)
    "foo" -> Literal("foo")
    2     -> Literal(2.0)

    Args:
        value: object. Python representation of the parsed literal value.
    """

    value: object

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_literal_expr(self)


@dataclass(eq=False, frozen=True)
class Logical(Expr):
    """Logical expression.

    Represents a logical expression which evaluates to a bool. Because both
    operands are expressions, logical expressions can be chained ie (a and b or c)

    For example:
    tokens = Scanner("true or false;").scan_tokens()
    statements = Parser(tokens).parse()
    print(statements[0])

    Expression(
        expression=Logical(
            left=Literal(True),
            operator=Token(TokenType.OR, "or", None, 1),
            right=Literal(False),
        ),
    )

    Args:
        left: Expr. Expression to evaluate for left operand.
        operator. Token. Token representing the logical operation to perform.
        right. Expr. Expression to evaluate for right operand.
    """

    left: Expr
    operator: Token
    right: Expr

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_logical_expr(self)


@dataclass(eq=False, frozen=True)
class Set(Expr):
    """Set expression.

    Represents the setting of a property on a given Lox object. Evaluates to the
    value being set.

    For example:
    tokens = Scanner("foo.bar = 2;").scan_tokens()
    statements = Parser(tokens).parse()
    print(statements[0])

    Expression(
        expression=Set(
            object=Variable(Token(TokenType.IDENTIFIER, "foo", None, 1)),
            name=Token(TokenType.IDENTIFIER, "bar", None, 1)),
            value=Literal(2.0),
        ),
    )

    Args:
        object: Expr. Expression to evaluate for the object to set properties
            on, this is an Expr rather than a Variable to allow for the chaining
            of multiple gets and a set ie foo().bar = 2
        name: Token. Token representing the property to set on the object.
        value: Expr. Expression to evaluate for property value.
    """

    object: Expr
    name: Token
    value: Expr

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_set_expr(self)


@dataclass(eq=False, frozen=True)
class Super(Expr):
    """Super expression.

    Represents the retrieval of a method on the superclass of a Lox class. The
    super expression is contextual evaluates to the superclass itself.

    For example:
    tokens = Scanner("super.bar();").scan_tokens()
    statements = Parser(tokens).parse()
    print(statements[0])

    Expression(
        expression=Call(
            callee=Super(
                keyword=Token(TokenType.SUPER, "super", None, 1),
                method=Token(TokenType.IDENTIFIER, "bar", None, 1),
            ),
            paren=Token(TokenType.RIGHT_PAREN, ")", None, 1),
            arguments=[],
            ),
        ),
    )

    The Super expression itself only represents the retrieval, so it will always
    be wrapped in a Call expression that actually invokes the retrieved method.

    Args:
        keyword: Token. Token representing the super keyword.
        method: Token. Token representing the method to retrieve on the superclass.
    """

    keyword: Token
    method: Token

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_super_expr(self)


@dataclass(eq=False, frozen=True)
class This(Expr):
    """This expression.

    A this expression is contextual and evaluates to the invoking Lox instance
    when used within a class method. It is analagous to the self parameter in
    Python.

    For example:
    tokens = Scanner("this.bar;").scan_tokens()
    statements = Parser(tokens).parse()
    print(statements[0])

    Expression(
        expression=Get(
            object=This(keyword=Token(TokenType.THIS, "this", None, 1)),
            name=Token(TokenType.IDENTIFIER, "bar", None, 1),
        ),
    )

    Args:
        keyword: Token. Token representing the this keyword.
    """

    keyword: Token

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_this_expr(self)


@dataclass(eq=False, frozen=True)
class Unary(Expr):
    """Unary expression.

    Represents a unary operation, either negation of a float (-2.0) or a boolean
    value (!true == false)

    For example:
    tokens = Scanner("-2;").scan_tokens()
    statements = Parser(tokens).parse()
    print(statements[0])

    Expression(
        expression=Unary(
            operator=Token(TokenType.MINUS, "-", None, 1),
            right=Literal(2.0),
        ),
    )

    Args:
        operator: Token. Token representing the unary operation to perform.
        right: Expr. Expression to evaluate for the right operand.
    """

    operator: Token
    right: Expr

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_unary_expr(self)


@dataclass(eq=False, frozen=True)
class Variable(Expr):
    """Variable expression.

    Represents a variable within a Lox source. Evaluates to the value stored in
    the variable at runtime.

    For example:
    tokens = Scanner("bar;").scan_tokens()
    statements = Parser(tokens).parse()
    print(statements[0])

    Expression(
        expression=Variable(name=Token(TokenType.IDENTIFIER, "bar", None, 1)),
    )

    Args:
        name: Token. Token representing the name of the variable.
    """

    name: Token

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_variable_expr(self)


class Visitor(Generic[R], metaclass=ABCMeta):
    """Lox Expression Visitor.

    This is a generic class which represents the interface for a Lox expression
    visitor. An implementing subclass must override visit methods for all Expr
    types in the Lox grammar, which are then called when the expression is
    "visited" via the accept method.

    The type parameter R indicates the type of values returned by the visitor
    methods of the implementing subclass.

    For example:
    class Printer(Visitor[str]):
        def print(self, expr: Expr) -> str:
            return expr.accept(self)

        def visit_assign_expr(self, expr: Assign) -> str:
            return f"assignment: {expr}"

    printer = Printer()
    expr = Assign(
        name=Token(TokenType.IDENTIFIER, "a", None, 1),
        value=Literal(2.0),
    )

    printer.print(expr)
    'assignment: Assign(name=TokenType.IDENTIFIER a None, value=Literal(value=2.0))'
    """

    @abstractmethod
    def visit_assign_expr(self, expr: Assign) -> R: ...

    @abstractmethod
    def visit_binary_expr(self, expr: Binary) -> R: ...

    @abstractmethod
    def visit_call_expr(self, expr: Call) -> R: ...

    @abstractmethod
    def visit_get_expr(self, expr: Get) -> R: ...

    @abstractmethod
    def visit_grouping_expr(self, expr: Grouping) -> R: ...

    @abstractmethod
    def visit_literal_expr(self, expr: Literal) -> R: ...

    @abstractmethod
    def visit_logical_expr(self, expr: Logical) -> R: ...

    @abstractmethod
    def visit_set_expr(self, expr: Set) -> R: ...

    @abstractmethod
    def visit_super_expr(self, expr: Super) -> R: ...

    @abstractmethod
    def visit_this_expr(self, expr: This) -> R: ...

    @abstractmethod
    def visit_unary_expr(self, expr: Unary) -> R: ...

    @abstractmethod
    def visit_variable_expr(self, expr: Variable) -> R: ...
