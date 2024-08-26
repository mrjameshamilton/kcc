package eu.jameshamilton.frontend

import eu.jameshamilton.frontend.StorageClass.EXTERN
import eu.jameshamilton.frontend.StorageClass.NONE

data class Program(val declarations: List<Declaration>)

typealias Block = List<BlockItem>


sealed class BlockItem
sealed class Statement : BlockItem()

sealed class Declaration(open val name: Identifier, open val storageClass: StorageClass = NONE) : BlockItem()
data class VarDeclaration(
    override val name: Identifier,
    val initializer: Expression?,
    override val storageClass: StorageClass = NONE
) : Declaration(name) {
    constructor(identifier: Identifier, storageClass: StorageClass) : this(identifier, null, storageClass)
}

data class FunDeclaration(
    override val name: Identifier,
    val params: List<VarDeclaration>?,
    val body: Block?,
    override val storageClass: StorageClass = EXTERN
) : Declaration(name)

enum class StorageClass {
    NONE, STATIC, EXTERN
}

enum class Type {
    INT
}

sealed class UnlabeledStatement : Statement()
data class LabeledStatement(val identifier: Identifier, val statement: Statement) : Statement()

sealed interface SwitchCase {
    val statement: Statement
    val label: Identifier?
}

data class ExpressionCase(
    val expression: Expression,
    override val statement: Statement,
    override val label: Identifier? = null
) : Statement(),
    SwitchCase

data class DefaultCase(override val statement: Statement, override val label: Identifier? = null) : Statement(),
    SwitchCase

data class ReturnStatement(val value: Expression) : UnlabeledStatement()
data class ExpressionStatement(val expression: Expression) : UnlabeledStatement()
data object NullStatement : UnlabeledStatement()
data class If(val condition: Expression, val thenBranch: Statement, val elseBranch: Statement? = null) :
    UnlabeledStatement()

data class Compound(val block: Block) : UnlabeledStatement()
data class Goto(val identifier: Identifier) : UnlabeledStatement()
data class Break(val identifier: Identifier? = null) : UnlabeledStatement()
data class Continue(val identifier: Identifier? = null) : UnlabeledStatement()

sealed interface Loop {
    val id: Identifier?
    val breakLabel: Identifier?
        get() = if (id == null) null else Identifier(id!!.identifier + "_break", id!!.line)
    val continueLabel: Identifier?
        get() = if (id == null) null else Identifier(id!!.identifier + "_continue", id!!.line)
}

data class While(val condition: Expression, val body: Statement, override val id: Identifier? = null) :
    UnlabeledStatement(), Loop

data class DoWhile(val condition: Expression, val body: Statement, override val id: Identifier? = null) :
    UnlabeledStatement(), Loop

data class For(
    val init: ForInit,
    val condition: Expression?,
    val post: Expression?,
    val body: Statement,
    override val id: Identifier? = null
) : UnlabeledStatement(), Loop

sealed interface ForInit
data class InitExpr(val expression: Expression?) : ForInit
data class InitDecl(val declaration: VarDeclaration) : ForInit

data class Switch(
    val expression: Expression,
    val statement: Statement,
    val id: Identifier? = null,
    val caseLabels: List<Identifier> = mutableListOf()
) :
    UnlabeledStatement() {
    val breakLabel: Identifier?
        get() = if (id == null) null else Identifier(id.identifier + "_break", id.line)
}

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

data class FunctionCall(val identifier: Identifier, val arguments: List<Expression> = emptyList()) : Expression()

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
