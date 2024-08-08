package eu.jameshamilton.frontend

data class Program(val function: FunctionDef)

data class FunctionDef(val name: Token, val body: List<Statement>)

sealed class Statement

data class ReturnStatement(val value: Expression) : Statement()

sealed class Expression

data class Constant(val value: Int) : Expression()

data class UnaryExpr(val op: UnaryOp, val expression: Expression) : Expression()

enum class UnaryOp {
    Complement, Negate
}

data class BinaryExpr(val left: Expression, val operator: BinaryOp, val right: Expression) : Expression()
enum class BinaryOp {
    Add, Subtract, Multiply, Divide, Remainder
}
