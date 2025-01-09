#!/usr/bin/env python3
from __future__ import annotations

from abc import ABCMeta, abstractmethod
from dataclasses import dataclass
from typing import Generic, List, Optional, TypeVar

from lox.expr import Expr, Variable
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
class Stmt(metaclass=ABCMeta):
    """Base class for a Lox statement.

    Like an expression, a statement is a sequence of Tokens. However, rather than
    evaluating to a value, a statement will have some side effect on the program,
    such as assigning a variable or printing it.

    For example:
    tokens = Scanner("print 2 + 2;").scan_tokens()
    statements = Parser(tokens).parse()

    Will yield a list with a single Print statement:
    Print(
        expression=Binary(
            left=Literal(2.0),
            right=Literal(2.0),
            operator=Token(TokenType.PLUS, "+", None, 1),
        ),
    )

    The internal Binary expression evaluates to the result of 2 + 2, while the
    Print statement takes the value from this expression and prints it.

    Statements in Lox are implemented using the visitor pattern. As such, all
    statements extend a single method, accept, which will route the statement
    to the correct visitor method on the invoking instance.
    """

    @abstractmethod
    def accept(self, visitor: Visitor[R]) -> R: ...


@dataclass(eq=False, frozen=True)
class Block(Stmt):
    """Block statement.

    Represents a collection of statements enclosed by braces {} which will have
    their own local variable scope within the interpreter.

    For example:
    tokens = Scanner("{ print 2; }").scan_tokens()
    statements = Parser(tokens).parse()
    print(statements[0])

    Block(statements=[Print(expression=Literal(value=2.0))])

    Args:
        statements: List[Stmt]. All statements included within this block.
    """

    statements: List[Stmt]

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_block_stmt(self)


@dataclass(eq=False, frozen=True)
class Class(Stmt):
    """Class statement.

    Represents the definition of a Lox class, which has the side effect of creating
    and registering it as a LoxClass in the interpreter's environment.

    For example:
    tokens = Scanner("class Foo < Bar { baz() { print 2; } }").scan_tokens()
    statements = Parser(tokens).parse()
    print(statements[0])

    Class(
        name=Token(TokenType.IDENTIFIER, "Foo", None, 1),
        superclass=Variable(name=Token(TokenType.IDENTIFIER, "bar", None, 1)),
        methods=[
            Function(
                name=Token(TokenType.IDENTIFIER, "baz", None, 1),
                params=[],
                body=[
                    Print(
                        expression=Literal(2.0),
                    ),
                ],
            ),
        ],
    )

    Args:
        name: Token. Token representing the name of the class.
        superclass: Optional[Variable]. Variable with the name of the superclass
            if any.
        methods: List[Function]. All methods defined on this class.
    """

    name: Token
    superclass: Optional[Variable]
    methods: List[Function]

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_class_stmt(self)


@dataclass(eq=False, frozen=True)
class Expression(Stmt):
    """Expression statement.

    Represents a "bare" expression statement, where the side effect is just
    evaluation of it.

    For example:
    tokens = Scanner("2;").scan_tokens()
    statements = Parser(tokens).parse()
    print(statements[0])

    Expression(expression=Literal(2.0))

    Args:
        expression: Expr. Inner expression to evaluate.
    """

    expression: Expr

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_expression_stmt(self)


@dataclass(eq=False, frozen=True)
class Function(Stmt):
    """Function statement.

    Represents the definition of a Lox function, which has the side effect of
    defining a function. Functions when called will have their own local scope
    that exists for the duration of the call.

    For example:
    tokens = Scanner("fun add(a) { return a + 1; }").scan_tokens()
    statements = Parser(tokens).parse()
    print(statements[0])

    Function(
        name=Token(TokenType.IDENTIFIER, "add", None, 1),
        params=[Token(TokenType.IDENTIFIER, "a", None, 1)],
        body=[
            Return(
                keyword=Token(TokenType.RETURN, "return", None, 1),
                value=Binary(,
                    left=Variable(Token(TokenType.IDENTIFIER, "a", None, 1),
                    operator=Token(TokenType.PLUS, "+", None, 1),
                    right=Literal(1.0),
                ),
            )
        ]
    )

    Functions can be defined in the global scope, local scopes, as well as methods
    on classes.

    Args:
        name: Token. Token representing the name of the function.
        params: List[Token]. Tokens representing the input parameters to the function.
        body: List[Stmt]. Statements comprising the function body.
    """

    name: Token
    params: List[Token]
    body: List[Stmt]

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_function_stmt(self)


@dataclass(eq=False, frozen=True)
class If(Stmt):
    """If statement.

    Represents an if statement, which has the side effect of changing control
    flow via evaluation of a condition expression, executing the then or else
    statements as necessary based on the result.

    source = "if (2 > 1) { return true; } else { return false; }"
    tokens = Scanner(source).scan_tokens()
    statements = Parser(tokens).parse()
    print(statements[0])

    If(
        condition=Binary(
            left=Literal(2.0),
            operator=Token(TokenType.GREATER, ">", None, 1),
            right=Literal(1.0),
        ),
        then_branch=Block(
            [
                Return(
                    keyword=Token(TokenType.RETURN, "return", None, 1),
                    value=Literal(True),
                ),
            ]
        ),
        else_branch=Block(
            [
                Return(
                    keyword=Token(TokenType.RETURN, "return", None, 1),
                    value=Literal(False),
                ),
            ]
        ),
    )

    Args:
        condition: Expr. Expression to evaluate for the if condition.
        then_branch: Stmt. Execute this statement if True.
        else_branch: Optional[Stmt]. If set, execute this statement if False.
    """

    condition: Expr
    then_branch: Stmt
    else_branch: Optional[Stmt] = None

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_if_stmt(self)


@dataclass(eq=False, frozen=True)
class Print(Stmt):
    """Print statement.

    Represents the built-in print statement, which has the side effect of printing
    the evaluated value from the expression to stdout.

    For example:
    tokens = Scanner("print 2;").scan_tokens()
    statements = Parser(tokens).parse()
    print(statements[0])

    Print(expression=Literal(2.0))

    Args:
        expression. Expr. Expression to evaluate for the value to print to stdout.
    """

    expression: Expr

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_print_stmt(self)


@dataclass(eq=False, frozen=True)
class Return(Stmt):
    """Return statement.

    Represents the built-in return statement, which has the side effect of stopping
    execution of a given function and returning a value if provided. This is
    implemented as a Python exception which is raised and caught by the
    interpreter while executing the body of a function.

    For example:
    tokens = Scanner("return true;").scan_tokens()
    statements = Parser(tokens).parse()
    print(statements[0])

    Return(keyword=Token(TokenType.RETURN, "return", None, 1), value=Literal(2.0))

    Args:
        keyword: Token. Token representing the return keyword.
        value: Optional[Expr]. Value to return to the caller, if any.
    """

    keyword: Token
    value: Optional[Expr] = None

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_return_stmt(self)


@dataclass(eq=False, frozen=True)
class Var(Stmt):
    """Var statement.

    Represents the definition of, and optionally the initialization of, a variable
    within Lox. This has the side effect of storing the initialized value in the
    Environment corresponding to the scope that the statement is encountered.

    For example:
    tokens = Scanner("var foo = 2;").scan_tokens()
    statements = Parser(tokens).parse()
    print(statements[0])

    Var(
        name=Token(TokenType.IDENTIFIER, "foo", None, 1),
        initializer=Literal(2.0),
    )

    Args:
        name: Token. Token representing the name of this new variable. This will
            shadow any variables with the same name defined at higher scopes.
        initializer: Optional[Expr]. Expression to evaluate for the initial value
            of the variable. If not provided, it will be set to nil (None).
    """

    name: Token
    initializer: Optional[Expr] = None

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_var_stmt(self)


@dataclass(eq=False, frozen=True)
class While(Stmt):
    """While statement.

    Represents a while loop, which has the side effect of continually evaluating
    the body statement as long as the condition expression evaluates to true.

    For example:
    tokens = Scanner("while (true) { print 2; }").scan_tokens()
    statements = Parser(tokens).parse()
    print(statements[0])

    While(
        condition=Literal(True),
        body=Block(
            statements=[
                Print(Literal(2.0)),
            ]
        ),
    )

    for loops in Lox are also implemented as while statements, such as:
    for (var a = 0; a > 3; a = a + 1) { print a; }

    Which is desugared into a Var statement, and a While with a block that increments:
    Var(Token(TokenType.IDENTIFIER, "a", None, 1), Literal(0)
    While(
        condition=Binary(
            left=Variable(Token(TokenType.IDENTIFIER, "a", None, 1)),
            operator=Token(TokenType.GREATER, ">", None, 1),
            right=Literal(3.0),
        ),
        body=Block(
            statements=[
                Print(Variable(Token(TokenType.IDENTIFIER, "a", None, 1))),
                Expression(
                    Assign(
                        name=Token(TokenType.IDENTIFIER, "a", None, 1),
                        value=Binary(
                            left=Variable(Token(TokenType.IDENTIFIER, "a", None, 1)),
                            operator=Token(TokenType.PLUS, "+", None, 1),
                            right=Literal(1.0),
                        ),
                    )
                ),
            ],
        ),
    )

    Args:
        condition: Expr. Expression to evaluate for body execution.
        body: Stmt. Execute this body statement each time the condition is true.
    """

    condition: Expr
    body: Stmt

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_while_stmt(self)


class Visitor(Generic[R], metaclass=ABCMeta):
    """Lox Statement Visitor.

    This is a generic class which represents the interface for a Lox statement
    visitor. An implementing subclass must override visit methods for all Stmt
    types in the Lox grammar, which are then called when a statement is "executed"
    via the accept method.

    The type parameter R indicates the type of values returned by the visitor
    methods of the implementing subclass. Since statements often contain expressions,
    both interfaces are usually necessary in an implementing subclass.

    For example:
    class Interpreter(ExprVisitor[object], StmtVisitor[None]):
        def evaluate(self, expr: Expr) -> object:
            return expr.accept(self)

        def execute(self, stmt: Stmt) -> None:
            return stmt.accept(self)

        def visit_literal_expr(self, expr: Literal) -> object:
            return expr.value

        def visit_print_stmt(self, stmt: Print) -> None:
            value = self.evaluate(stmt.expression)
            print(value)

    interpreter = Interpreter()
    stmt = Print(
        expression=Literal(2.0),
    )

    interpreter.execute(stmt)
    2.0
    """

    @abstractmethod
    def visit_block_stmt(self, stmt: Block) -> R: ...

    @abstractmethod
    def visit_class_stmt(self, stmt: Class) -> R: ...

    @abstractmethod
    def visit_expression_stmt(self, stmt: Expression) -> R: ...

    @abstractmethod
    def visit_function_stmt(self, stmt: Function) -> R: ...

    @abstractmethod
    def visit_if_stmt(self, stmt: If) -> R: ...

    @abstractmethod
    def visit_print_stmt(self, stmt: Print) -> R: ...

    @abstractmethod
    def visit_return_stmt(self, stmt: Return) -> R: ...

    @abstractmethod
    def visit_var_stmt(self, stmt: Var) -> R: ...

    @abstractmethod
    def visit_while_stmt(self, stmt: While) -> R: ...
