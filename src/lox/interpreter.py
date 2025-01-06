#!/usr/bin/env python3
from __future__ import annotations

from math import isnan
from time import time
from typing import Dict, List, cast

from lox.environment import Environment
from lox.expr import (
    Assign,
    Binary,
    Call,
    Expr,
    Get,
    Grouping,
    Literal,
    Logical,
    Set,
    Super,
    This,
    Unary,
    Variable,
)
from lox.expr import Visitor as ExprVisitor
from lox.lox import Lox
from lox.lox_callable import LoxCallable
from lox.lox_class import LoxClass
from lox.lox_function import LoxFunction
from lox.lox_instance import LoxInstance
from lox.lox_return import Return
from lox.runtime_error import LoxRuntimeError
from lox.stmt import (
    Block,
    Class,
    Expression,
    Function,
    If,
    Print,
    Stmt,
    Var,
    While,
)
from lox.stmt import Return as ReturnStmt
from lox.stmt import Visitor as StmtVisitor
from lox.token import Token
from lox.token_type import TokenType


class Interpreter(ExprVisitor[object], StmtVisitor[None]):
    def __init__(self) -> None:
        self.globals = Environment()
        self.environment: Environment = self.globals
        self.locals: Dict[Expr, int] = {}

        # Native function clock(), which returns the current system time in
        # seconds as a double.
        class ClockBuiltIn(LoxCallable):
            def __repr__(self) -> str:
                return self.to_string()

            def arity(self) -> int:
                return 0

            def call(self, interpreter: Interpreter, arguments: List[object]) -> object:
                # Convert to seconds
                return time() / 1000.0

            def to_string(self) -> str:
                return "<native fn>"

        self.globals.define("clock", ClockBuiltIn())

    def interpret(self, statements: List[Stmt]) -> None:
        try:
            for statement in statements:
                self.execute(statement)
        except LoxRuntimeError as error:
            Lox.runtime_error(error)

    def visit_literal_expr(self, expr: Literal) -> object:
        return expr.value

    def visit_logical_expr(self, expr: Logical) -> object:
        # Evaluate left hand of the logical expression
        left = self.evaluate(expr.left)

        if expr.operator.type is TokenType.OR:
            # Short circuit if it is an OR and the left is truthy
            if self.is_truthy(left):
                return left
        else:
            # Short circuit if it is an AND and the left is not falsey
            if not self.is_truthy(left):
                return left

        # Continue to evaluate the right hand of the logical expression
        return self.evaluate(expr.right)

    def visit_set_expr(self, expr: Set) -> object:
        obj = self.evaluate(expr.object)

        if not isinstance(obj, LoxInstance):
            raise LoxRuntimeError(expr.name, "Only instances have fields.")

        value = self.evaluate(expr.value)
        obj.set(expr.name, value)

        return value

    def visit_super_expr(self, expr: Super) -> object:
        # Look up the "super" variable which is automatically defined in an
        # enclosing scope during the instance method call if a class has a
        # superclass. See LoxClass.findMethod for more info
        distance = self.locals[expr]

        # Retrieve "super" as the class which is the direct superclass of the
        # class which contains this super keyword (not necessarily the same as
        # the class referred to by the "this" instance)
        superclass = cast(LoxClass, self.environment.get_at(distance, "super"))

        # Retrieve "this" as the class instance which is invoking this method,
        # which is always in the environment immediately below the one
        # containing super
        obj = cast(LoxInstance, self.environment.get_at(distance - 1, "this"))

        # Find the method on the retrieved superclass and return a function
        # which is bound to the retrieved "this" class instance
        method = superclass.find_method(expr.method.lexeme)

        # Superclass doesn't have this method
        if method is None:
            raise LoxRuntimeError(
                expr.method, f"Undefined property '{expr.method.lexeme}'."
            )

        return method.bind(obj)

    def visit_this_expr(self, expr: This) -> object:
        # Look up the "this" variable which is automatically defined in an
        # enclosing scope during the instance method call, see
        # LoxFunction.bind for more info
        return self.look_up_variable(expr.keyword, expr)

    def visit_unary_expr(self, expr: Unary) -> object:
        """Interpret a unary value.

        Args:
            expr: Unary. Expression to interpret.

        Returns:
            value: object. Value of the unary expression.
        """

        # Evaluate the right side of the expression
        # ie the true in !true, where true is a literal expr
        right = self.evaluate(expr.right)

        match expr.operator.type:
            case TokenType.BANG:
                # Negate the right value
                # !true == false
                return not self.is_truthy(right)
            case TokenType.MINUS:
                # Ensure the evaluated right value is a number, and make negative
                self.check_number_operand(expr.operator, right)
                return -cast(float, right)
            case _:
                return None

    def visit_variable_expr(self, expr: Variable) -> object:
        return self.look_up_variable(expr.name, expr)

    def look_up_variable(self, name: Token, expr: Expr) -> object:
        # Get the resolved distance up the scope stack for this variable
        distance = self.locals.get(expr)

        if distance is not None:
            # Retrieve the value for the variable in the correct enviironment
            return self.environment.get_at(distance, name.lexeme)
        else:
            # No distance, this is a global variable
            return self.globals.get(name)

    def check_number_operand(self, operator: Token, operand: object) -> None:
        """Check whether the operand to a given operator is a number.

        All numbers in Lox are floats (Java Doubles), so this is just a check
        for whether or not the provided operand is of type float.

        Args:
            operator: Token. Token for operation being performed.
            operand: object. Operand value being checked for the operation

        Raises:
            LoxRuntimeError: If the operand is not a number (float).
        """

        if isinstance(operand, float):
            return

        raise LoxRuntimeError(operator, "Operand must be a number.")

    def check_number_operands(
        self, operator: Token, left: object, right: object
    ) -> None:
        """Check whether the operands to a binary expression are both numbers.

        Both operands evaluated and checked at once, rather than left to right.

        Args:
            operator: Token. Token for binary operation being performed.
            left: object. Left value to the binary operation.
            right: object. Right value to the binary operation.

        Raises:
            LoxRuntimeError: If either the left or right operand is not a number.
        """

        if isinstance(left, float) and isinstance(right, float):
            return

        raise LoxRuntimeError(operator, "Operands must be numbers.")

    def visit_grouping_expr(self, expr: Grouping) -> object:
        """Interpret a grouping expression.

        This invokes the expression's visitor method, interpreting it and
        returning the value. For example, (2 + 2) would visit the binary
        expression 2 + 2's visitor, returning the result 4. This will recurisvely
        evaluate any other subsequent groupings as needed.

        Args:
            expr: Grouping. Grouped expression to evaluate.

        Returns:
            result: object. Result of the expression evaluation.
        """

        return self.evaluate(expr.expression)

    def evaluate(self, expr: Expr) -> object:
        """Evaluate an expression, visiting and returning the value.

        Args:
            expr: Expr. Expression to evaluate.

        Returns:
            result: object. Result of expression evaluation.
        """

        return expr.accept(self)

    def execute(self, stmt: Stmt) -> None:
        stmt.accept(self)

    def resolve(self, expr: Expr, depth: int) -> None:
        self.locals[expr] = depth

    def execute_block(self, statements: List[Stmt], environment: Environment) -> None:
        previous = self.environment

        try:
            self.environment = environment

            for statement in statements:
                self.execute(statement)
        finally:
            self.environment = previous

    def visit_block_stmt(self, stmt: Block) -> None:
        self.execute_block(stmt.statements, Environment(self.environment))

    def visit_class_stmt(self, stmt: Class) -> None:
        # Ensure the superclass is actually a class, if supplied
        superclass = None
        if stmt.superclass is not None:
            superclass = self.evaluate(stmt.superclass)
            if not isinstance(superclass, LoxClass):
                # Redundant assertion to make type checker happy
                assert stmt.superclass is not None
                raise LoxRuntimeError(
                    stmt.superclass.name, "Superclass must be a class."
                )

        self.environment.define(stmt.name.lexeme, None)

        # Create the environment holding "super" and set it to the superclas
        if stmt.superclass is not None:
            self.environment = Environment(self.environment)
            self.environment.define("super", superclass)

        # Evaluate all methods on this class
        methods = {}
        for method in stmt.methods:
            # Conditionally set initializer flag since this is a method
            # definition
            function = LoxFunction(
                method, self.environment, method.name.lexeme == "init"
            )
            methods[method.name.lexeme] = function

        klass = LoxClass(stmt.name.lexeme, superclass, methods)

        if superclass is not None:
            # Redundant assertion to make type checker happy
            assert self.environment.enclosing is not None
            # Restore the enclosing environment if there is a superclass
            self.environment = self.environment.enclosing

        self.environment.assign(stmt.name, klass)

    def visit_expression_stmt(self, stmt: Expression) -> None:
        self.evaluate(stmt.expression)

    def visit_function_stmt(self, stmt: Function) -> None:
        # Store the Environment from function declaration time
        # Set isInitializer to false since this is a function and not a
        # class method declaration
        function = LoxFunction(stmt, self.environment, False)

        self.environment.define(stmt.name.lexeme, function)

    def visit_if_stmt(self, stmt: If) -> None:
        if self.is_truthy(self.evaluate(stmt.condition)):
            self.execute(stmt.then_branch)
        elif stmt.else_branch is not None:
            self.execute(stmt.else_branch)

    def visit_print_stmt(self, stmt: Print) -> None:
        value = self.evaluate(stmt.expression)
        print(self.stringify(value))

    def visit_return_stmt(self, stmt: ReturnStmt) -> None:
        value = None
        if stmt.value is not None:
            # Evaluate the return statement's value if it has one
            value = self.evaluate(stmt.value)

        # Throw an exception with the return value to unwind the stack back to
        # the call site of the function which is returning
        raise Return(value)

    def visit_var_stmt(self, stmt: Var) -> None:
        value = None
        if stmt.initializer is not None:
            value = self.evaluate(stmt.initializer)

        self.environment.define(stmt.name.lexeme, value)

    def visit_while_stmt(self, stmt: While) -> None:
        while self.is_truthy(self.evaluate(stmt.condition)):
            self.execute(stmt.body)

    def visit_assign_expr(self, expr: Assign) -> object:
        value = self.evaluate(expr.value)

        distance = self.locals.get(expr)
        if distance is not None:
            self.environment.assign_at(distance, expr.name, value)
        else:
            self.globals.assign(expr.name, value)

        return value

    def visit_binary_expr(self, expr: Binary) -> object:
        left = self.evaluate(expr.left)
        right = self.evaluate(expr.right)

        match expr.operator.type:
            # Comparison operators, this will return a bool
            case TokenType.GREATER:
                self.check_number_operands(expr.operator, left, right)
                return cast(float, left) > cast(float, right)
            case TokenType.GREATER_EQUAL:
                self.check_number_operands(expr.operator, left, right)
                return cast(float, left) >= cast(float, right)
            case TokenType.LESS:
                self.check_number_operands(expr.operator, left, right)
                return cast(float, left) < cast(float, right)
            case TokenType.LESS_EQUAL:
                self.check_number_operands(expr.operator, left, right)
                return cast(float, left) <= cast(float, right)
            # Equality operators, these will return a bool
            case TokenType.BANG_EQUAL:
                return not self.is_equal(left, right)
            case TokenType.EQUAL_EQUAL:
                return self.is_equal(left, right)
            # Arithmetic operators, these will return a float or str
            case TokenType.MINUS:
                self.check_number_operands(expr.operator, left, right)
                return cast(float, left) - cast(float, right)
            case TokenType.PLUS:
                # Arithmetic
                # 2 + 2 = 4
                if isinstance(left, float) and isinstance(right, float):
                    return left + right

                # String concatenation
                # "foo" + "bar" = "foobar"
                if isinstance(left, str) and isinstance(right, str):
                    return left + right

                raise LoxRuntimeError(
                    expr.operator, "Operands must be two numbers or two strings."
                )
            case TokenType.SLASH:
                self.check_number_operands(expr.operator, left, right)
                return cast(float, left) / cast(float, right)
            case TokenType.STAR:
                self.check_number_operands(expr.operator, left, right)
                return cast(float, left) * cast(float, right)
            # Unreachable
            case _:
                return None

    def visit_call_expr(self, expr: Call) -> object:
        # Evaluate the expression which yields the callee
        callee = self.evaluate(expr.callee)

        # Evaluate all argument expressions and and add the results
        arguments = []
        for argument in expr.arguments:
            arguments.append(self.evaluate(argument))

        # Ensure the callee can be called (implements calling interface)
        # "foobar"(); <- RuntimeError, since String does not implement LoxCallable
        if not isinstance(callee, LoxCallable):
            raise LoxRuntimeError(expr.paren, "Can only call functions and classes.")

        # Cast to a LoxCallable and call it, returning the result
        # Pyre said the cast was redundant so it's just a reassignment here
        function = callee

        # Check the arity of the function and fail if number of args do not match
        # fun add(a, b, c) { print a + b + c; }
        # arity = 3
        # add(1, 2, 3, 4); <- RuntimeError
        # add(1, 2) <- RuntimeError
        if len(arguments) != function.arity():
            raise LoxRuntimeError(
                expr.paren,
                f"Expected {function.arity()} arguments but got {len(arguments)}.",
            )

        return function.call(self, arguments)

    def visit_get_expr(self, expr: Get) -> object:
        obj = self.evaluate(expr.object)

        if isinstance(obj, LoxInstance):
            return obj.get(expr.name)

        raise LoxRuntimeError(expr.name, "Only instances have properties.")

    def is_truthy(self, obj: object) -> bool:
        """Return the "truthiness" of a given value.

        Truthiness in the case of Lox follows the Ruby pattern, where nil (null)
        is false, and all other values (other than boolean False) are true.

        Args:
            obj: object. Lox object to evaluate.

        Returns:
            truthy: bool. Truthiness of Lox object.
        """

        # nil (null) == false
        if obj is None:
            return False
        # object is a boolean already
        if isinstance(obj, bool):
            return obj

        # All other values are truthy
        return True

    def is_equal(self, a: object, b: object) -> bool:
        """Evaluate the equality of two Lox objects.

        This uses each Python type's __eq__ method underneath to evaluate
        the equality of Lox objects.

        In Java's Double.equals, NaN == NaN, which does not match the IEEE
        spec. In Python, float("nan") != float("nan") as expected, so this
        method changes the behavior to match Java for consistency.

        Additionally, comparison between float 0/1 and boolean True/False are
        corrected to match Java's behavior and Lox's expectation.

        Args:
            a: object. Left operand.
            b: object. Right operand.

        Returns:
            equal: bool. Equality result.
        """

        # nil == nil
        if a is None and b is None:
            return True
        # (nil == expression) == false
        if a is None:
            return False
        # (nan == nan) == true
        if (isinstance(a, float) and isnan(a)) and (isinstance(b, float) and isnan(b)):
            return True
        # (0 == false) == false
        # (true == 1) == false
        # In Python, these are both True, so it has to be handled here
        # See munificent/craftinginterpreters/test/operator/equals.lox
        if (isinstance(a, bool) and isinstance(b, float)) or (
            isinstance(a, float) and isinstance(b, bool)
        ):
            return False

        # Use __eq__ method
        return a == b

    def stringify(self, obj: object) -> str:
        """Generate a string representation of a Lox object.

        Args:
            obj: object. Lox object.

        Returns:
            string: str. Provided object represented as a string.
        """

        # null -> "nil"
        if obj is None:
            return "nil"

        if isinstance(obj, float):
            text = str(obj)

            # "2.0" -> "2"
            if text.endswith(".0"):
                text = text[0:-2]

            return text

        if isinstance(obj, bool):
            # Required to match Java
            # True -> true
            # See munificent/craftinginterpreters/test/bool/*.lox
            return str(obj).lower()

        return str(obj)
