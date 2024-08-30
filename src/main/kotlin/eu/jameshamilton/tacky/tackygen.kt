package eu.jameshamilton.tacky

import eu.jameshamilton.frontend.Assignment
import eu.jameshamilton.frontend.BinaryExpr
import eu.jameshamilton.frontend.BinaryOp
import eu.jameshamilton.frontend.BinaryOp.Add
import eu.jameshamilton.frontend.BinaryOp.And
import eu.jameshamilton.frontend.BinaryOp.Divide
import eu.jameshamilton.frontend.BinaryOp.Equal
import eu.jameshamilton.frontend.BinaryOp.GreaterThan
import eu.jameshamilton.frontend.BinaryOp.GreaterThanOrEqual
import eu.jameshamilton.frontend.BinaryOp.LeftShift
import eu.jameshamilton.frontend.BinaryOp.LessThan
import eu.jameshamilton.frontend.BinaryOp.LessThanOrEqual
import eu.jameshamilton.frontend.BinaryOp.LogicalAnd
import eu.jameshamilton.frontend.BinaryOp.LogicalOr
import eu.jameshamilton.frontend.BinaryOp.Multiply
import eu.jameshamilton.frontend.BinaryOp.NotEqual
import eu.jameshamilton.frontend.BinaryOp.Or
import eu.jameshamilton.frontend.BinaryOp.Remainder
import eu.jameshamilton.frontend.BinaryOp.RightShift
import eu.jameshamilton.frontend.BinaryOp.Subtract
import eu.jameshamilton.frontend.BinaryOp.Xor
import eu.jameshamilton.frontend.BlockItem
import eu.jameshamilton.frontend.Break
import eu.jameshamilton.frontend.Cast
import eu.jameshamilton.frontend.Compound
import eu.jameshamilton.frontend.Conditional
import eu.jameshamilton.frontend.Constant
import eu.jameshamilton.frontend.Continue
import eu.jameshamilton.frontend.DefaultCase
import eu.jameshamilton.frontend.DoWhile
import eu.jameshamilton.frontend.Expression
import eu.jameshamilton.frontend.ExpressionCase
import eu.jameshamilton.frontend.ExpressionStatement
import eu.jameshamilton.frontend.For
import eu.jameshamilton.frontend.FunDeclaration
import eu.jameshamilton.frontend.FunType
import eu.jameshamilton.frontend.FunctionCall
import eu.jameshamilton.frontend.Goto
import eu.jameshamilton.frontend.If
import eu.jameshamilton.frontend.InitDecl
import eu.jameshamilton.frontend.InitExpr
import eu.jameshamilton.frontend.IntType
import eu.jameshamilton.frontend.LabeledStatement
import eu.jameshamilton.frontend.LongType
import eu.jameshamilton.frontend.NullStatement
import eu.jameshamilton.frontend.Program
import eu.jameshamilton.frontend.ReturnStatement
import eu.jameshamilton.frontend.Switch
import eu.jameshamilton.frontend.SwitchCase
import eu.jameshamilton.frontend.Type
import eu.jameshamilton.frontend.UnaryExpr
import eu.jameshamilton.frontend.UnaryOp
import eu.jameshamilton.frontend.UnaryOp.PostfixDecrement
import eu.jameshamilton.frontend.UnaryOp.PostfixIncrement
import eu.jameshamilton.frontend.UnaryOp.PrefixDecrement
import eu.jameshamilton.frontend.UnaryOp.PrefixIncrement
import eu.jameshamilton.frontend.Unknown
import eu.jameshamilton.frontend.Var
import eu.jameshamilton.frontend.VarDeclaration
import eu.jameshamilton.frontend.While
import eu.jameshamilton.frontend.check.FunAttr
import eu.jameshamilton.frontend.check.Initial
import eu.jameshamilton.frontend.check.LocalAttr
import eu.jameshamilton.frontend.check.NoInitializer
import eu.jameshamilton.frontend.check.StaticAttr
import eu.jameshamilton.frontend.check.SymbolTableEntry
import eu.jameshamilton.frontend.check.Tentative
import eu.jameshamilton.frontend.check.resolveSwitchCases
import eu.jameshamilton.frontend.check.symbolTable
import eu.jameshamilton.unreachable
import eu.jameshamilton.tacky.Binary as TackyBinary
import eu.jameshamilton.tacky.BinaryOp as TackyBinaryOp
import eu.jameshamilton.tacky.BinaryOp.Add as TackyBinaryOpAdd
import eu.jameshamilton.tacky.Constant as TackyConstant
import eu.jameshamilton.tacky.FunctionCall as TackyFunctionCall
import eu.jameshamilton.tacky.FunctionDef as TackyFunctionDef
import eu.jameshamilton.tacky.Label as TackyLabel
import eu.jameshamilton.tacky.Program as TackyProgram
import eu.jameshamilton.tacky.Unary as TackyUnary
import eu.jameshamilton.tacky.UnaryOp as TackyUnaryOp
import eu.jameshamilton.tacky.Var as TackyVar

fun convert(program: Program): TackyProgram {
    val functions =
        program.declarations
            .filterIsInstance<FunDeclaration>()
            .filter { it.body != null }
            .map { convert(it) }

    val staticVariables: List<StaticVariable> =
        symbolTable
            .filterValues { it.attr is StaticAttr }
            .map { Triple(it.key, it.value.type, it.value.attr as StaticAttr) }
            .mapNotNull { (name, type, attr) ->
                when (attr.initialValue) {
                    is Initial -> StaticVariable(name, attr.global, type, attr.initialValue.value)
                    Tentative -> StaticVariable(
                        name, attr.global, type, when (type) {
                            is IntType -> 0
                            is LongType -> 0L
                            else -> unreachable("invalid type $type")
                        }
                    )

                    NoInitializer -> null
                }
            }

    return TackyProgram(staticVariables + functions)
}

private var count = 0
private fun maketemporary(type: Type): TackyVar {
    val name = "tmp.${count++}"
    symbolTable[name] = SymbolTableEntry(type, LocalAttr)
    return TackyVar(type, name)
}

private var labels = 0
private fun makelabel(name: String): LabelIdentifier = "${name}_${labels++}"

private fun convert(funDeclaration: FunDeclaration): TackyFunctionDef {

    fun convert(op: UnaryOp): TackyUnaryOp = when (op) {
        UnaryOp.Complement -> TackyUnaryOp.Complement
        UnaryOp.Negate -> TackyUnaryOp.Negate
        UnaryOp.Not -> TackyUnaryOp.Not
        PrefixIncrement, PostfixIncrement, PrefixDecrement, PostfixDecrement -> unreachable("special case")
    }

    val switches = resolveSwitchCases(funDeclaration)

    fun convert(operator: BinaryOp): TackyBinaryOp = when (operator) {
        Add -> TackyBinaryOp.Add
        Subtract -> TackyBinaryOp.Subtract
        Multiply -> TackyBinaryOp.Multiply
        Divide -> TackyBinaryOp.Divide
        Remainder -> TackyBinaryOp.Remainder
        And -> TackyBinaryOp.And
        Or -> TackyBinaryOp.Or
        Xor -> TackyBinaryOp.Xor
        LeftShift -> TackyBinaryOp.LeftShift
        RightShift -> TackyBinaryOp.RightShift
        LogicalAnd, LogicalOr -> unreachable("special case")
        Equal -> TackyBinaryOp.Equal
        NotEqual -> TackyBinaryOp.NotEqual
        LessThan -> TackyBinaryOp.LessThan
        GreaterThan -> TackyBinaryOp.GreaterThan
        LessThanOrEqual -> TackyBinaryOp.LessThanOrEqual
        GreaterThanOrEqual -> TackyBinaryOp.GreaterThanOrEqual
    }

    fun convert(instructions: MutableList<Instruction>, expression: Expression): Value = when (expression) {
        is Constant -> TackyConstant(expression.value)
        is UnaryExpr -> buildTacky(instructions) {
            val src = convert(instructions, expression.expression)
            when (expression.op) {
                PostfixIncrement -> {
                    val dst = maketemporary(expression.type)
                    copy(src, dst)
                    increment(src)
                    dst
                }

                PostfixDecrement -> {
                    val dst = maketemporary(expression.type)
                    copy(src, dst)
                    increment(src, -1)
                    dst
                }

                PrefixIncrement -> increment(src)
                PrefixDecrement -> increment(src, -1)
                else -> {
                    val dst = maketemporary(expression.type)
                    val op = convert(expression.op)
                    unaryOp(op, src, dst)
                }
            }
        }

        is BinaryExpr -> when (expression.operator) {
            LogicalAnd -> buildTacky(instructions) {
                val dst = maketemporary(expression.type)
                val falseLabel = makelabel("and_false_label")
                val endLabel = makelabel("and_end_label")

                val v1 = convert(instructions, expression.left)
                jumpIfZero(v1, falseLabel)
                val v2 = convert(instructions, expression.right)
                jumpIfZero(v2, falseLabel)
                copy(1, dst)
                jump(endLabel)
                label(falseLabel)
                copy(0, dst)
                label(endLabel)
                dst
            }

            LogicalOr -> buildTacky(instructions) {
                val dst = maketemporary(expression.type)
                val falseLabel = makelabel("or_false_label")
                val endLabel = makelabel("or_end_label")

                val v1 = convert(instructions, expression.left)
                jumpIfNotZero(v1, falseLabel)
                val v2 = convert(instructions, expression.right)
                jumpIfNotZero(v2, falseLabel)
                copy(0, dst)
                jump(endLabel)
                label(falseLabel)
                copy(1, dst)
                label(endLabel)
                dst
            }

            else -> buildTacky(instructions) {
                val v1 = convert(instructions, expression.left)
                val v2 = convert(instructions, expression.right)
                val dst = maketemporary(expression.type)
                val tackyOp = convert(expression.operator)
                binaryOp(tackyOp, v1, v2, dst)
                dst
            }
        }

        is Assignment -> buildTacky(instructions) {
            val value = convert(instructions, expression.value)
            val lvalue = convert(instructions, expression.lvalue)
            copy(value, lvalue)
            value
        }

        is Var -> TackyVar(expression.type, expression.identifier.identifier)
        is Conditional -> buildTacky(instructions) {
            val result = maketemporary(expression.type)
            val condition = convert(instructions, expression.condition)
            val elseLabel = makelabel("else_label")
            val endLabel = makelabel("end_label")
            jumpIfZero(condition, elseLabel)
            val e1 = convert(instructions, expression.thenBranch)
            copy(e1, result)
            jump(endLabel)
            label(elseLabel)
            val e2 = convert(instructions, expression.elseBranch)
            copy(e2, result)
            label(endLabel)
            result
        }

        is FunctionCall -> buildTacky(instructions) {
            val arguments = expression.arguments.map { convert(instructions, it) }
            val result = maketemporary(expression.type)
            call(expression.identifier.identifier, arguments, result)
            result
        }

        is Cast -> buildTacky(instructions) {
            val result = convert(instructions, expression.expression)
            if (expression.targetType == expression.expression.type) {
                nop()
            } else {
                val dst = maketemporary(expression.type)
                symbolTable[dst.name] = SymbolTableEntry(expression.targetType, LocalAttr)
                when (expression.targetType) {
                    is FunType -> unreachable("cast to function type not possible")
                    IntType -> truncate(result, dst)
                    LongType -> signextend(result, dst)
                    Unknown -> unreachable("${expression.targetType} unknown")
                }
                dst
            }
        }
    }

    fun convert(instructions: MutableList<Instruction>, statement: BlockItem) {
        buildTacky(instructions) {
            when (statement) {
                is ReturnStatement -> ret(convert(instructions, statement.value))
                is ExpressionStatement -> convert(instructions, statement.expression)
                is NullStatement -> emptyList<Instruction>()
                is VarDeclaration -> {
                    if (symbolTable[statement.name.identifier]?.attr is LocalAttr && statement.initializer != null) {
                        val src = convert(instructions, statement.initializer)
                        val dst = TackyVar(statement.type, statement.name.identifier)
                        copy(src, dst)
                    }
                }

                is FunDeclaration -> {
                    if (statement.body != null) {
                        unreachable("local functions not supported")
                    }
                }

                is If -> {
                    val endLabel = makelabel("if_end")
                    val elseLabel = if (statement.elseBranch == null) endLabel else makelabel("else_label")
                    val condition = convert(instructions, statement.condition)
                    jumpIfZero(condition, elseLabel)
                    convert(instructions, statement.thenBranch)
                    if (statement.elseBranch != null) {
                        jump(endLabel)
                        label(elseLabel)
                        convert(instructions, statement.elseBranch)
                    }
                    label(endLabel)
                }

                is Goto -> jump(statement.identifier.identifier)
                is LabeledStatement -> {
                    label(statement.identifier.identifier)
                    convert(instructions, statement.statement)
                }

                is Compound -> statement.block.forEach { convert(instructions, it) }
                is Break -> jump(statement.identifier!!.identifier)
                is Continue -> jump(statement.identifier!!.identifier)
                is DoWhile -> {
                    assert(statement.id != null)

                    val startLabel = makelabel("start")
                    val continueLabel = statement.continueLabel!!.identifier
                    val breakLabel = statement.breakLabel!!.identifier

                    label(startLabel)
                    convert(instructions, statement.body)
                    label(continueLabel)
                    val result = convert(instructions, statement.condition)
                    jumpIfNotZero(result, startLabel)
                    label(breakLabel)
                }

                is While -> {
                    assert(statement.id != null)

                    val continueLabel = statement.continueLabel!!.identifier
                    val breakLabel = statement.breakLabel!!.identifier

                    label(continueLabel)
                    val result = convert(instructions, statement.condition)
                    jumpIfZero(result, breakLabel)
                    convert(instructions, statement.body)
                    jump(continueLabel)
                    label(breakLabel)
                }

                is For -> {
                    when (statement.init) {
                        is InitDecl -> convert(instructions, statement.init.declaration)
                        is InitExpr -> statement.init.expression?.let { convert(instructions, it) }
                    }
                    val startLabel = makelabel("start")
                    val continueLabel = statement.continueLabel!!.identifier
                    val breakLabel = statement.breakLabel!!.identifier

                    label(startLabel)
                    if (statement.condition != null) {
                        val result = convert(instructions, statement.condition)
                        jumpIfZero(result, breakLabel)
                    }
                    convert(instructions, statement.body)
                    label(continueLabel)
                    statement.post?.let { convert(instructions, it) }
                    jump(startLabel)
                    label(breakLabel)
                }

                is Switch -> {
                    require(statement.id != null)
                    require(switches.containsKey(statement))

                    val switchValue = convert(instructions, statement.expression)
                    val cases = switches[statement]

                    require(cases != null)

                    if (cases.isNotEmpty()) {
                        require(statement.breakLabel != null)

                        val breakLabel = statement.breakLabel!!.identifier
                        val temp = maketemporary(statement.expression.type)

                        cases.forEach { case ->
                            require(case.label != null)
                            val target = case.label!!.identifier

                            when (case) {
                                is ExpressionCase -> {
                                    val caseValue = convert(instructions, case.expression)
                                    equal(switchValue, caseValue, temp)
                                    jumpIfNotZero(temp, target)
                                }

                                is DefaultCase -> {
                                    jump(target)
                                }
                            }
                        }

                        if (cases.count { it is DefaultCase } == 0) {
                            jump(breakLabel)
                        }

                        convert(instructions, statement.statement)

                        label(breakLabel)
                    }
                    nop()
                }

                is SwitchCase -> {
                    label(statement.label!!.identifier)
                    convert(instructions, statement.statement)
                    nop()
                }
            }
            nop()
        }
    }

    fun convert(statements: List<BlockItem>?): List<Instruction> = statements?.flatMap { blockItem ->
        val instructions = mutableListOf<Instruction>()
        convert(instructions, blockItem)
        instructions
    } ?: emptyList()

    val attr = symbolTable[funDeclaration.name.identifier]?.attr as FunAttr

    return TackyFunctionDef(
        funDeclaration.type,
        funDeclaration.name.identifier,
        attr.global,
        funDeclaration.body != null,
        funDeclaration.params?.map { it.name.identifier } ?: emptyList(),
        // TODO: don't add return for declarations?
        convert(funDeclaration.body) + listOf(Return(TackyConstant(0)))
    )
}

class Builder(private val instructions: MutableList<Instruction> = mutableListOf()) {
    fun jump(target: String): Value {
        instructions += Jump(target)
        return nop()
    }

    fun jumpIfZero(condition: Value, target: String) {
        instructions += JumpIfZero(condition, target)
    }

    fun jumpIfNotZero(condition: Value, target: String) {
        instructions += JumpIfNotZero(condition, target)
    }

    fun label(label: LabelIdentifier): Value {
        instructions += TackyLabel(label)
        return nop()
    }

    fun copy(src: Value, dst: Value): Value {
        instructions += Copy(src, dst)
        return dst
    }

    fun copy(i: Int, dst: Value): Value {
        instructions += Copy(TackyConstant(i), dst)
        return dst
    }

    fun binaryOp(binaryOp: TackyBinaryOp, src1: Value, src2: Value, dst: Value): Value {
        instructions += TackyBinary(binaryOp, src1, src2, dst)
        return dst
    }

    fun equal(src1: Value, src2: Value, dst: Value): Value {
        return binaryOp(TackyBinaryOp.Equal, src1, src2, dst)
    }

    fun increment(src: Value, amount: Int = 1): Value {
        return increment(src, TackyConstant(amount))
    }

    fun increment(src: Value, amount: Value): Value {
        instructions += TackyBinary(TackyBinaryOpAdd, src, amount, src)
        return src
    }

    fun unaryOp(unaryOp: TackyUnaryOp, src: Value, dst: Value): Value {
        instructions += TackyUnary(unaryOp, src, dst)
        return dst
    }

    fun ret(value: Value) {
        instructions += Return(value)
    }

    fun call(identifier: String, arguments: List<Value>, result: Value): Value {
        instructions += TackyFunctionCall(identifier, arguments, result)
        return result
    }

    fun signextend(src: Value, dst: Value): Value {
        instructions += SignExtend(src, dst)
        return dst
    }

    fun truncate(src: Value, dst: Value): Value {
        instructions += Truncate(src, dst)
        return dst
    }

    fun nop(): Value = TackyConstant(0)
}

fun buildTacky(instructions: MutableList<Instruction>, block: Builder.() -> Value): Value =
    with(Builder(instructions)) {
        return block(this)
    }
