#!/usr/bin/env python3
from __future__ import annotations

from math import isnan
from time import time
from typing import Dict, List, Union, cast

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
    """Lox interpreter.

    This class executes a set of statements returned by the Parser which
    represent a Lox source. This represents the runtime execution of Lox where
    all expressions and side effects are interpreted into their Python equivalents.

    For example:
    # Scan source file
    tokens = Scanner("a = 2; print a;").scan_tokens()

    # Parse source tokens into statements
    statements = Parser(tokens).parse()

    # Create interpreter and resolver
    interpreter = Interpreter()
    resolver = Resolver(interpreter)

    # Resolve all variables
    resolver.resolve(statements)

    # Execute source
    interpreter.interpret(statements)

    2

    Public Attributes:
        globals: Environment. Environment holding global variable state, including
            built-in functions.
        environment: Environment. The environment for the current scope. This
            starts as globals and then changes as the interpreter enters and
            exits scopes.
        locals: Dict[Expr, int]. Mapping of expressions requiring local variable
            lookups encountered in the Resolver to their "depth" in the scope
            tree. This is used to traverse the environment linked list when
            resolving a variable value at runtime.
    """

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
        """Interpret a set of statements.

        This is the entrypoint for the Interpreter, which will execute the
        statements in the Interpreter's environment.

        All encountered statements will be executed using execute(), producing
        any runtime side effects such as printing values to stdout.

        All encountered expressions within statements will be evaluated using
        evalute(), producing their runtime values.

        Args:
            statements: List[Stmt]. Statements to interpret.
        """

        try:
            for statement in statements:
                self.execute(statement)
        except LoxRuntimeError as error:
            Lox.runtime_error(error)

    def visit_literal_expr(self, expr: Literal) -> object:
        """Evaluate a literal expression.

        This returns the value that was eagerly parsed prior to runtime and stored
        in the Literal instance itself.

        Args:
            expr: Literal. Literal expression to evaluate.

        Returns:
            value: object. Inner value of the literal expression.
        """

        return expr.value

    def visit_logical_expr(self, expr: Logical) -> object:
        """Evaluate a logical expression.

        This evaluates the left value of the expression, and the right, unless
        the operation short circuits.

        Args:
            expr: Logical. Logical expression to evaluate.

        Returns:
            value: object. Result of evaluating the left or right operand, based
                on the result of the logical operation.
        """

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
        """Evaluate a set expression.

        This evaluates the object being assigned and sets the given value on it.

        Args:
            expr: Set. Set expression to evaluate.

        Returns:
            value: object. The evaluated value set on the object.

        Raises:
            LoxRuntimeError: If the expression attempts to access fields on
                anything other than a class instance.
        """

        obj = self.evaluate(expr.object)

        if not isinstance(obj, LoxInstance):
            raise LoxRuntimeError(expr.name, "Only instances have fields.")

        value = self.evaluate(expr.value)
        obj.set(expr.name, value)

        return value

    def visit_super_expr(self, expr: Super) -> LoxFunction:
        """Evaluate a super expression.

        This resolves the superclass in the enclosing Environment, as well as the
        current instance, then binds the method on the superclass to it. See
        visit_class_stmt for more info.

        Args:
            expr: Super. Super expression to evaluate.

        Returns:
            superclass_function: LoxFunction. Method from the superclass bound
                to the invoking instance.

        Raises:
            LoxRuntimeError: If the superclass does not have a method with the
                name provided.
        """

        # Look up the "super" variable which is automatically defined in an
        # enclosing scope during the instance method call if a class has a
        # superclass. See LoxClass.find_method for more info
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

    def visit_this_expr(self, expr: This) -> LoxInstance:
        """Evaluate a this expression.

        This will look up the "this" variable which is automatically defined in
        an enclosing scope during the instance method call, see LoxFunction.bind
        and visit_class_stmt for more info.

        Args:
            expr: This. This expression to evaluate.

        Returns:
            this_instance: LoxInstance. The class instance being referenced.
        """

        # Look up the "this" variable
        return cast(LoxInstance, self.look_up_variable(expr.keyword, expr))

    def visit_unary_expr(self, expr: Unary) -> object:
        """Evaluate a unary expression.

        Args:
            expr: Unary. Unary expression to evaluate.

        Returns:
            value: object. Result of the unary expression.
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
        """Evaluate a variable expression.

        This will resolve the runtime value for a given variable expression in
        either the global or local scope. See look_up_variable for more info.

        Args:
            expr: Variable. Variable expression to evaluate.

        Returns:
            value: object. Runtime value for variable expression at this scope.
        """

        return self.look_up_variable(expr.name, expr)

    def look_up_variable(self, name: Token, expr: Expr) -> object:
        """Look up a variable.

        Given an expression which requires a variable value (Variable/This/Super),
        this will use the information populated by the Resolver to locate the
        closest local scope which contains a variable whose name matches the one
        referenced in the expression.

        If the variable cannot be found in any local scope, then the global scope
        is checked. This means that a locally defined variable with the same name
        will shadow one defined at any higher scope ie:
        fun shadow() {
            print clock();
            fun clock() {
                return "clock";
            }
            print clock();
        }
        shadow();

        plox examples/shadow.lox
        1736414.0000749
        clock

        Args:
            name: Token. Token representing the name of the variable to look up.
            expr: Expr. Expression (Variable/This/Super), which requires look up.

        Returns:
            value: object. The runtime value retrieved from the Environment of
                the resolved scope which contained a variable of the same name
                during the Resolver process.
        """

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
        """Evaluate a grouping expression.

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
        """Evaluate a statement, which returns no value but may have side effects.

        Args:
            stmt: Stmt. Statement to execute.
        """

        stmt.accept(self)

    def resolve(self, expr: Expr, depth: int) -> None:
        """Resolve the Environment depth for a given expression.

        This is called by the Resolver process prior to runtime execution and
        records the location of a local variable in the scope chain for a given
        expression requiring resolution (Variable/This/Super).

        Args:
            expr: Expr. Expression to resolve depth for.
            depth: int. The depth in the Environment scope chain which will
                contain the variable with a name matching the one in this expr.
        """

        self.locals[expr] = depth

    def execute_block(self, statements: List[Stmt], environment: Environment) -> None:
        """Execute a block of statements.

        This is the runtime equivalent to the Resolver's begin_scope() method. An
        Environment is created for the new block scope at the end of the scope
        tree. All statements are then executed in this new Environment. If an
        expression requires a runtime value (Variable/This/Super), the Resolver's
        scope data will be used to locate the correct Environment. See the
        resolve() and look_up_variable() methods for more info.

        Args:
            statements: List[Stmt]. Statements to execute in the new block scope.
            environment: Environment. New Environment to place at the end of the
                scope chain to hold this scope's local variable values.
        """

        # Store previous runtime scope's Environment.
        previous = self.environment

        try:
            # Begin new runtime scope, setting the supplied Environment as current.
            self.environment = environment

            # Execute all statements in this runtime scope
            for statement in statements:
                self.execute(statement)
        finally:
            # End scope, restoring the stored previous scope's Environment.
            self.environment = previous

    def visit_block_stmt(self, stmt: Block) -> None:
        """Execute a block statement.

        This will execute a block of statements, creating a new runtime Environment
        for it. See execute_block() for more info.

        Args:
            stmt: Block. Block statement to execute.
        """

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
        """Execute an expression statement.

        This evaluates the inner expression as a side effect, but does not return
        the value.

        Args:
            stmt: Expression. Expression statement to execute.
        """

        self.evaluate(stmt.expression)

    def visit_function_stmt(self, stmt: Function) -> None:
        """Execute a function statement.

        This creates the LoxFunction runtime representation of the function and
        defines it in the enclosing environment.

        Args:
            stmt: Function. Function statement to execute.
        """

        # Store the Environment from function declaration time
        # Set is_initializer to false since this is a function and not a
        # class method declaration
        function = LoxFunction(stmt, self.environment, False)

        self.environment.define(stmt.name.lexeme, function)

    def visit_if_stmt(self, stmt: If) -> None:
        """Execute an if statement.

        This evaluates the condition expression, and executes the then or else
        branches based on the result.

        Args:
            stmt: If. If statement to execute.
        """

        if self.is_truthy(self.evaluate(stmt.condition)):
            self.execute(stmt.then_branch)
        elif stmt.else_branch is not None:
            self.execute(stmt.else_branch)

    def visit_print_stmt(self, stmt: Print) -> None:
        """Execute a print statement.

        This evaluates the expression and prints it to stdout using Python's
        built-in print() function.

        Args:
            stmt: Print. Print statement to execute.
        """

        value = self.evaluate(stmt.expression)
        print(self.stringify(value))

    def visit_return_stmt(self, stmt: ReturnStmt) -> None:
        """Execute a return statement.

        This evaluates the expression to be returned, if any, and raises a
        special Return exception with the value. This is caught and handled
        internally by LoxFunction.call, extracting the value and returning it
        to the Python call-site in visit_call_expr. See lox_return.Return for more
        info.

        Args:
            stmt: Return. Return statement to execute.
        """

        value = None
        if stmt.value is not None:
            # Evaluate the return statement's value if it has one
            value = self.evaluate(stmt.value)

        # Throw an exception with the return value to unwind the stack back to
        # the call site of the function which is returning
        raise Return(value)

    def visit_var_stmt(self, stmt: Var) -> None:
        """Execute a var statement.

        This defines a new variable in the current scope's Environment and
        evaluates/assigns the initializer if one is provided.

        Args:
            stmt: Var. Var statement to execute.
        """

        value = None
        if stmt.initializer is not None:
            value = self.evaluate(stmt.initializer)

        self.environment.define(stmt.name.lexeme, value)

    def visit_while_stmt(self, stmt: While) -> None:
        """Execute a while statement.

        This will continually execute the statement body so long as the statement
        condition evaluates to a truthy value after each execution.

        Args:
            stmt: While. While statement to execute.
        """

        while self.is_truthy(self.evaluate(stmt.condition)):
            self.execute(stmt.body)

    def visit_assign_expr(self, expr: Assign) -> object:
        """Execute an assignment expression.

        This will resolve the closest local (or global) scope with the variable
        referenced in this expression and assign the value of the evaluated
        expression.

        Args:
            expr: Assign. Assignment expression to execute.
        """

        value = self.evaluate(expr.value)

        distance = self.locals.get(expr)
        if distance is not None:
            self.environment.assign_at(distance, expr.name, value)
        else:
            self.globals.assign(expr.name, value)

        return value

    def visit_binary_expr(self, expr: Binary) -> Union[float, bool, str]:
        """Evaluate a binary expression.

        This handles all binary comparison operations at runtime, evaluating
        both the left and right sides and returning a value of the expected
        type based on the operation and operands.

        Args:
            binary: Binary. Binary expression to evaluate.

        Returns:
            value: Union[float, bool, str]. Python primitive value representing
                the result of the binary expression.

        Raises:
            LoxRuntimeError: If an unhandled binary operator is encountered.
        """

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
                raise LoxRuntimeError(expr.operator, "Unknown binary operator")

    def visit_call_expr(self, expr: Call) -> object:
        """Evaluate a call expression.

        This evaluates the callee and any arguments, then calls the resolved
        callee, returning the value from it if any.

        Callee evaluation can yield either a LoxFunction or a LoxClass, both
        of which implement the LoxCallable base class. See the respective classes
        for more info on call behavior.

        When calling, execution is handed over to the calling instance itself,
        which will create the Environment and execute all statements. Additionally,
        it will capture the Lox Return exception which is raised when a Return
        statement is encountered in a Callable's body, returning the value.

        Args:
            expr: Call. Call expression to evaluate.

        Returns:
            value: object. Result of the call.

        Raises:
            LoxRuntimeError: If the callee resolves to a non-callable value which
                does not implement the LoxCallable interface, or if the incorrect
                number of arguments is supplied to the call.
        """

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
        """Evaluate a get expression.

        This evaluates the Lox class instance which is being accessed and retrieves
        the property from it.

        Args:
            expr: Get. Get expression to evaluate.

        Returns:
            result: object. Retrieved runtime value for Lox instance property.
        """

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
