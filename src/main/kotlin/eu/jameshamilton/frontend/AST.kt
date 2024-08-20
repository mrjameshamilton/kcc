package eu.jameshamilton.frontend

data class Program(val function: FunctionDef)

typealias Block = List<BlockItem>

data class FunctionDef(val name: Identifier, val body: Block)

sealed class BlockItem
sealed class Statement : BlockItem()

sealed class UnlabeledStatement : Statement()
data class LabeledStatement(val identifier: Identifier, val statement: Statement) : Statement()
data class Label(val identifier: Identifier) : BlockItem()

data class ReturnStatement(val value: Expression) : UnlabeledStatement()
data class ExpressionStatement(val expression: Expression) : UnlabeledStatement()
data object NullStatement : UnlabeledStatement()
data class If(val condition: Expression, val thenBranch: Statement, val elseBranch: Statement? = null) :
    UnlabeledStatement()

data class Compound(val block: Block) : UnlabeledStatement()
data class Goto(val identifier: Identifier) : UnlabeledStatement()
data class Break(val identifier: Identifier? = null) : UnlabeledStatement()
data class Continue(val identifier: Identifier? = null) : UnlabeledStatement()

data class While(val condition: Expression, val body: Statement, val label: Identifier? = null) :
    UnlabeledStatement()

data class DoWhile(val condition: Expression, val body: Statement, val label: Identifier? = null) :
    UnlabeledStatement()

data class For(
    val init: ForInit,
    val condition: Expression?,
    val post: Expression?,
    val body: Statement,
    val label: Identifier? = null
) : UnlabeledStatement()

sealed interface ForInit
data class InitExpr(val expression: Expression?) : ForInit
data class InitDecl(val declaration: Declaration) : ForInit

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

data class Declaration(val identifier: Identifier, val initializer: Expression?) : BlockItem()

class Identifier(val identifier: String, val line: Int) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as Identifier

        return identifier == other.identifier
    }

    override fun hashCode(): Int = identifier.hashCode()

    override fun toString(): String = identifier
}
