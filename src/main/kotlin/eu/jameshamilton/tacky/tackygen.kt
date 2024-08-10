package eu.jameshamilton.tacky

import eu.jameshamilton.codegen.unreachable
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
import eu.jameshamilton.frontend.Constant
import eu.jameshamilton.frontend.Expression
import eu.jameshamilton.frontend.FunctionDef
import eu.jameshamilton.frontend.Program
import eu.jameshamilton.frontend.ReturnStatement
import eu.jameshamilton.frontend.Statement
import eu.jameshamilton.frontend.UnaryExpr
import eu.jameshamilton.frontend.UnaryOp
import eu.jameshamilton.tacky.Binary
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
        is UnaryExpr -> {
            val src = convert(expression.expression)
            val dst = Var(maketemporary())
            val op = convert(expression.op)
            instructions += buildTacky {
                unaryOp(op, src, dst)
            }
            dst
        }

        is BinaryExpr -> {
            if (expression.operator == LogicalAnd || expression.operator == LogicalOr) {
                val dst = Var(maketemporary())
                instructions += buildTacky {
                    val falseLabel = makelabel("false_label")
                    val endLabel = makelabel("end_label")

                    val v1 = convert(expression.left)
                    if (expression.operator == LogicalAnd)
                        jumpIfZero(v1, falseLabel)
                    else
                        jumpIfNotZero(v1, falseLabel)

                    val v2 = convert(expression.right)
                    if (expression.operator == LogicalAnd)
                        jumpIfZero(v2, falseLabel)
                    else
                        jumpIfNotZero(v2, falseLabel)

                    copy(1, dst)
                    jump(endLabel)
                    label(falseLabel)
                    copy(0, dst)
                    label(endLabel)
                }
                dst
            } else {
                val v1 = convert(expression.left)
                val v2 = convert(expression.right)
                val dst = Var(maketemporary())
                val tackyOp = convert(expression.operator)
                instructions += buildTacky {
                    binaryOp(tackyOp, v1, v2, dst)
                }
                dst
            }
        }
    }

    fun convert(statement: Statement): List<Instruction> {
        when (statement) {
            is ReturnStatement -> instructions += TackyReturn(convert(statement.value))
        }
        return instructions
    }

    fun convert(statements: List<Statement>): List<Instruction> = statements.flatMap(::convert)

    return TackyFunctionDef(program.name.lexeme, convert(program.body))
}

class Builder(val instructions: MutableList<Instruction> = mutableListOf()) {
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

fun buildTacky(block: Builder.() -> Unit): List<Instruction> = with(Builder()) {
    block(this)
    return instructions
}
