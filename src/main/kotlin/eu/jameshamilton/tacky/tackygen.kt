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
import eu.jameshamilton.frontend.Constant
import eu.jameshamilton.frontend.Declaration
import eu.jameshamilton.frontend.Expression
import eu.jameshamilton.frontend.ExpressionStatement
import eu.jameshamilton.frontend.FunctionDef
import eu.jameshamilton.frontend.NullStatement
import eu.jameshamilton.frontend.Program
import eu.jameshamilton.frontend.ReturnStatement
import eu.jameshamilton.frontend.UnaryExpr
import eu.jameshamilton.frontend.UnaryOp
import eu.jameshamilton.unreachable
import eu.jameshamilton.tacky.Binary as TackyBinary
import eu.jameshamilton.tacky.BinaryOp as TackyBinaryOp
import eu.jameshamilton.tacky.Constant as TackyConstant
import eu.jameshamilton.tacky.FunctionDef as TackyFunctionDef
import eu.jameshamilton.tacky.Program as TackyProgram
import eu.jameshamilton.tacky.Unary as TackyUnary
import eu.jameshamilton.tacky.UnaryOp as TackyUnaryOp

fun convert(program: Program): TackyProgram = TackyProgram(convert(program.function))

private fun convert(program: FunctionDef): TackyFunctionDef {

    val instructions = mutableListOf<Instruction>()

    fun convert(op: UnaryOp): TackyUnaryOp = when (op) {
        UnaryOp.Complement -> TackyUnaryOp.Complement
        UnaryOp.Negate -> TackyUnaryOp.Negate
        UnaryOp.Not -> TackyUnaryOp.Not
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

    fun convert(expression: Expression): Value = when (expression) {
        is Constant -> TackyConstant(expression.value)
        is UnaryExpr -> buildTacky(instructions) {
            val src = convert(expression.expression)
            val dst = Var(maketemporary())
            val op = convert(expression.op)
            unaryOp(op, src, dst)
            dst
        }

        is BinaryExpr -> when (expression.operator) {
            LogicalAnd -> buildTacky(instructions) {
                val dst = Var(maketemporary())
                val falseLabel = makelabel("false_label")
                val endLabel = makelabel("end_label")

                val v1 = convert(expression.left)
                jumpIfZero(v1, falseLabel)
                val v2 = convert(expression.right)
                jumpIfZero(v2, falseLabel)
                copy(1, dst)
                jump(endLabel)
                label(falseLabel)
                copy(0, dst)
                label(endLabel)
                dst
            }

            LogicalOr -> buildTacky(instructions) {
                val dst = Var(maketemporary())
                val falseLabel = makelabel("false_label")
                val endLabel = makelabel("end_label")

                val v1 = convert(expression.left)
                jumpIfNotZero(v1, falseLabel)
                val v2 = convert(expression.right)
                jumpIfNotZero(v2, falseLabel)
                copy(0, dst)
                jump(endLabel)
                label(falseLabel)
                copy(1, dst)
                label(endLabel)
                dst
            }

            else -> buildTacky(instructions) {
                val v1 = convert(expression.left)
                val v2 = convert(expression.right)
                val dst = Var(maketemporary())
                val tackyOp = convert(expression.operator)
                binaryOp(tackyOp, v1, v2, dst)
                dst
            }
        }

        is Assignment -> TODO()
        is eu.jameshamilton.frontend.Var -> TODO()
    }

    fun convert(statement: BlockItem): List<Instruction> {
        when (statement) {
            is ReturnStatement -> instructions += TackyReturn(convert(statement.value))
            is ExpressionStatement -> TODO()
            is NullStatement -> TODO()
            is Declaration -> TODO()
        }
        return instructions
    }

    fun convert(statements: List<BlockItem>): List<Instruction> = statements.flatMap(::convert)

    return TackyFunctionDef(program.name.identifier, convert(program.body))
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
        instructions += Label(label)
    }

    fun copy(src: Value, dst: Value) {
        instructions += Copy(src, dst)
    }

    fun copy(i: Int, dst: Value) {
        instructions += Copy(TackyConstant(i), dst)
    }

    fun binaryOp(binaryOp: TackyBinaryOp, src1: Value, src2: Value, dst: Value) {
        instructions += TackyBinary(binaryOp, src1, src2, dst)
    }

    fun unaryOp(unaryOp: TackyUnaryOp, src: Value, dst: Value) {
        instructions += TackyUnary(unaryOp, src, dst)
    }
}

fun buildTacky(instructions: MutableList<Instruction>, block: Builder.() -> Value): Value =
    with(Builder(instructions)) {
        return block(this)
    }
