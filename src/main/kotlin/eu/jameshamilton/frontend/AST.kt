package eu.jameshamilton.frontend

data class Program(val function: FunctionDef)

typealias Block = List<BlockItem>

data class FunctionDef(val name: Identifier, val body: Block)

interface BlockItem
sealed class Statement : BlockItem

data class ReturnStatement(val value: Expression) : Statement()
data class ExpressionStatement(val expression: Expression) : Statement()
data object NullStatement : Statement()
data class If(val condition: Expression, val thenBranch: Statement, val elseBranch: Statement? = null) : Statement()
data class Compound(val block: Block) : Statement()

sealed class Expression

data class Constant(val value: Int) : Expression()

data class UnaryExpr(val op: UnaryOp, val expression: Expression) : Expression()

enum class UnaryOp {
    Complement, Negate, Not, PrefixIncrement, PostfixIncrement, PrefixDecrement, PostfixDecrement
}

data class BinaryExpr(val left: Expression, val operator: BinaryOp, val right: Expression) : Expression()
enum class BinaryOp {
    Add, Subtract, Multiply, Divide, Remainder,
    And, Or, Xor, LeftShift, RightShift,
    LogicalAnd, LogicalOr, Equal, NotEqual, LessThan, GreaterThan, LessThanOrEqual, GreaterThanOrEqual
}

data class Var(val identifier: Identifier) : Expression()
data class Assignment(val lvalue: Expression, val value: Expression) : Expression()
data class Conditional(val condition: Expression, val thenBranch: Expression, val elseBranch: Expression) :
    Expression()

data class Declaration(val identifier: Identifier, val initializer: Expression? = null) : BlockItem

data class Identifier(val identifier: String, val line: Int)
