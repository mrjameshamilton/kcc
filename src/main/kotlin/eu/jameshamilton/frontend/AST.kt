package eu.jameshamilton.frontend

import eu.jameshamilton.frontend.StorageClass.EXTERN
import eu.jameshamilton.frontend.StorageClass.NONE

data class Program(val declarations: List<Declaration>)

typealias Block = List<BlockItem>


sealed class BlockItem
sealed class Statement : BlockItem()

sealed class Declaration(open val name: Identifier, open val type: Type, open val storageClass: StorageClass = NONE) :
    BlockItem()

data class VarDeclaration(
    override val name: Identifier,
    val initializer: Initializer?,
    override val type: Type,
    override val storageClass: StorageClass = NONE
) : Declaration(name, type) {
    constructor(identifier: Identifier, type: Type, storageClass: StorageClass) : this(
        identifier,
        null,
        type,
        storageClass
    )
}

sealed class Initializer(override val type: Type = Unknown) : Expression(type)
data class SingleInit(val expression: Expression, override val type: Type = Unknown) : Initializer(type)
data class CompoundInit(val expressions: List<Initializer>, override val type: Type = Unknown) : Initializer(type)

data class FunDeclaration(
    override val name: Identifier,
    val params: List<VarDeclaration>?,
    val body: Block?,
    override val type: FunType,
    override val storageClass: StorageClass = EXTERN
) : Declaration(name, type)

enum class StorageClass {
    NONE, STATIC, EXTERN
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

data class ReturnStatement(val value: Expression? = null) : UnlabeledStatement()
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

sealed class Expression(open val type: Type = Unknown)

data class Constant(val value: Any, override val type: Type = Unknown) : Expression(type)
data class StringConstant(val value: String, override val type: Type = Unknown) : Expression(type), Assignable

data class UnaryExpr(val op: UnaryOp, val expression: Expression, override val type: Type = Unknown) : Expression(type)

enum class UnaryOp(private val c: String) {
    Complement("~"),
    Negate("-"),
    Not("!"),
    PrefixIncrement("++"),
    PostfixIncrement("++"),
    PrefixDecrement("--"),
    PostfixDecrement("--");

    override fun toString(): String = c
}

data class BinaryExpr(
    val left: Expression,
    val operator: BinaryOp,
    val right: Expression,
    override val type: Type = Unknown
) : Expression(type)

enum class BinaryOp(private val c: String) {
    Add("+"),
    Subtract("-"),
    Multiply("*"),
    Divide("/"),
    Remainder("%"),
    And("&"),
    Or("|"),
    Xor("^"),
    LeftShift("<<"),
    RightShift(">>"),
    LogicalAnd("&&"),
    LogicalOr("||"),
    Equal("=="),
    NotEqual("!="),
    LessThan("<"),
    GreaterThan(">"),
    LessThanOrEqual("<="),
    GreaterThanOrEqual(">=");

    override fun toString(): String = c
}

sealed interface Assignable
data class Var(val identifier: Identifier, override val type: Type = Unknown) : Expression(type), Assignable
data class Assignment(
    val lvalue: Expression,
    val rvalue: Expression,
    val compound: Boolean = false,
    override val type: Type = Unknown
) :
    Expression(type)

data class Conditional(
    val condition: Expression, val thenBranch: Expression, val elseBranch: Expression,
    override val type: Type = Unknown
) :
    Expression(type)

data class Cast(val targetType: Type, val expression: Expression, override val type: Type = Unknown) : Expression(type)
data class FunctionCall(
    val identifier: Identifier, val arguments: List<Expression> = emptyList(),
    override val type: Type = Unknown
) : Expression(type)

data class Dereference(val expression: Expression, override val type: Type = Unknown) : Expression(type), Assignable
data class AddrOf(val expression: Expression, override val type: Type = Unknown) : Expression(type)
data class Subscript(val expr1: Expression, val expr2: Expression, override val type: Type = Unknown) :
    Expression(type), Assignable
data class SizeOf(val expression: Expression, override val type: Type = Unknown) : Expression(type)
data class SizeOfT(val t: Type, override val type: Type = Unknown) : Expression(type)

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

val Expression.isLValue: Boolean
    get() = this is Assignable
