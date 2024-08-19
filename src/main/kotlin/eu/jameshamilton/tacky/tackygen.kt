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
import eu.jameshamilton.frontend.Compound
import eu.jameshamilton.frontend.Conditional
import eu.jameshamilton.frontend.Constant
import eu.jameshamilton.frontend.Declaration
import eu.jameshamilton.frontend.Expression
import eu.jameshamilton.frontend.ExpressionStatement
import eu.jameshamilton.frontend.FunctionDef
import eu.jameshamilton.frontend.Goto
import eu.jameshamilton.frontend.If
import eu.jameshamilton.frontend.Label
import eu.jameshamilton.frontend.LabeledStatement
import eu.jameshamilton.frontend.NullStatement
import eu.jameshamilton.frontend.Program
import eu.jameshamilton.frontend.ReturnStatement
import eu.jameshamilton.frontend.UnaryExpr
import eu.jameshamilton.frontend.UnaryOp
import eu.jameshamilton.frontend.UnaryOp.PostfixDecrement
import eu.jameshamilton.frontend.UnaryOp.PostfixIncrement
import eu.jameshamilton.frontend.UnaryOp.PrefixDecrement
import eu.jameshamilton.frontend.UnaryOp.PrefixIncrement
import eu.jameshamilton.frontend.Var
import eu.jameshamilton.unreachable
import eu.jameshamilton.tacky.Binary as TackyBinary
import eu.jameshamilton.tacky.BinaryOp as TackyBinaryOp
import eu.jameshamilton.tacky.BinaryOp.Add as TackyBinaryOpAdd
import eu.jameshamilton.tacky.Constant as TackyConstant
import eu.jameshamilton.tacky.FunctionDef as TackyFunctionDef
import eu.jameshamilton.tacky.Label as TackyLabel
import eu.jameshamilton.tacky.Program as TackyProgram
import eu.jameshamilton.tacky.Unary as TackyUnary
import eu.jameshamilton.tacky.UnaryOp as TackyUnaryOp
import eu.jameshamilton.tacky.Var as TackyVar

fun convert(program: Program): TackyProgram = TackyProgram(convert(program.function))

private fun convert(program: FunctionDef): TackyFunctionDef {

    fun convert(op: UnaryOp): TackyUnaryOp = when (op) {
        UnaryOp.Complement -> TackyUnaryOp.Complement
        UnaryOp.Negate -> TackyUnaryOp.Negate
        UnaryOp.Not -> TackyUnaryOp.Not
        PrefixIncrement, PostfixIncrement, PrefixDecrement, PostfixDecrement -> unreachable("special case")
    }

    var count = 0
    fun maketemporary(): String = "tmp.${count++}"
    var labels = 0
    fun makelabel(name: String): LabelIdentifier = "${name}_${labels++}"

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
                    val dst = TackyVar(maketemporary())
                    copy(src, dst)
                    increment(src)
                    dst
                }

                PostfixDecrement -> {
                    val dst = TackyVar(maketemporary())
                    copy(src, dst)
                    increment(src, -1)
                    dst
                }

                PrefixIncrement -> increment(src)
                PrefixDecrement -> increment(src, -1)
                else -> {
                    val dst = TackyVar(maketemporary())
                    val op = convert(expression.op)
                    unaryOp(op, src, dst)
                }
            }
        }

        is BinaryExpr -> when (expression.operator) {
            LogicalAnd -> buildTacky(instructions) {
                val dst = TackyVar(maketemporary())
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
                val dst = TackyVar(maketemporary())
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
                val dst = TackyVar(maketemporary())
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

        is Var -> TackyVar(expression.identifier.identifier)
        is Conditional -> buildTacky(instructions) {
            val result = TackyVar(maketemporary())
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
    }

    fun convert(statement: BlockItem): List<Instruction> {
        val instructions = mutableListOf<Instruction>()
        buildTacky(instructions) {
            when (statement) {
                is ReturnStatement -> ret(convert(instructions, statement.value))
                is ExpressionStatement -> convert(instructions, statement.expression)
                is NullStatement -> emptyList<Instruction>()
                is Declaration -> if (statement.initializer != null) {
                    val src = convert(instructions, statement.initializer)
                    val dst = TackyVar(statement.identifier.identifier)
                    copy(src, dst)
                }

                is If -> {
                    val endLabel = makelabel("if_end")
                    val elseLabel = if (statement.elseBranch == null) endLabel else makelabel("else_label")
                    val condition = convert(instructions, statement.condition)
                    jumpIfZero(condition, elseLabel)
                    instructions += convert(statement.thenBranch)
                    if (statement.elseBranch != null) {
                        jump(endLabel)
                        label(elseLabel)
                        instructions += convert(statement.elseBranch)
                    }
                    label(endLabel)
                }

                is Goto -> jump(statement.identifier.identifier)
                is LabeledStatement -> {
                    label(statement.identifier.identifier)
                    instructions += convert(statement.statement)
                }

                // TODO: refactor instructions += for statements?
                is Compound -> instructions += statement.block.flatMap { convert(it) }
                is Label -> label(statement.identifier.identifier)
            }
            nop()
        }
        return instructions
    }

    fun convert(statements: List<BlockItem>): List<Instruction> {
        return statements.flatMap { convert(it) }
    }

    return TackyFunctionDef(program.name.identifier, convert(program.body) + listOf(TackyReturn(TackyConstant(0))))
}

class Builder(private val instructions: MutableList<Instruction> = mutableListOf()) {
    fun jump(target: String) {
        instructions += Jump(target)
    }

    fun jumpIfZero(condition: Value, target: String) {
        instructions += JumpIfZero(condition, target)
    }

    fun jumpIfNotZero(condition: Value, target: String) {
        instructions += JumpIfNotZero(condition, target)
    }

    fun label(label: LabelIdentifier) {
        instructions += TackyLabel(label)
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
        instructions += TackyReturn(value)
    }

    fun nop(): Value = TackyConstant(0)
}

fun buildTacky(instructions: MutableList<Instruction>, block: Builder.() -> Value): Value =
    with(Builder(instructions)) {
        return block(this)
    }
