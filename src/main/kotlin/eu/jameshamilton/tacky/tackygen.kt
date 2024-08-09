package eu.jameshamilton.tacky

import eu.jameshamilton.frontend.BinaryExpr
import eu.jameshamilton.frontend.BinaryOp
import eu.jameshamilton.frontend.Constant
import eu.jameshamilton.frontend.Expression
import eu.jameshamilton.frontend.FunctionDef
import eu.jameshamilton.frontend.Program
import eu.jameshamilton.frontend.ReturnStatement
import eu.jameshamilton.frontend.Statement
import eu.jameshamilton.frontend.UnaryExpr
import eu.jameshamilton.frontend.UnaryOp
import eu.jameshamilton.tacky.Binary as TackyBinary
import eu.jameshamilton.tacky.BinaryOp as TackyBinaryOp
import eu.jameshamilton.tacky.Constant as TackyConstant
import eu.jameshamilton.tacky.FunctionDef as TackyFunctionDef
import eu.jameshamilton.tacky.Program as TackyProgram
import eu.jameshamilton.tacky.Unary as TackyUnary

fun convert(program: Program): TackyProgram = TackyProgram(convert(program.function))

private fun convert(program: FunctionDef): TackyFunctionDef {

    val instructions = mutableListOf<Instruction>()

    fun convert(op: UnaryOp): eu.jameshamilton.tacky.UnaryOp = when (op) {
        UnaryOp.Complement -> eu.jameshamilton.tacky.UnaryOp.Complement
        UnaryOp.Negate -> eu.jameshamilton.tacky.UnaryOp.Negate
    }

    var count = 0
    fun maketemporary(): String = "tmp.${count++}"

    fun convert(operator: BinaryOp): TackyBinaryOp = when (operator) {
        BinaryOp.Add -> TackyBinaryOp.Add
        BinaryOp.Subtract -> TackyBinaryOp.Subtract
        BinaryOp.Multiply -> TackyBinaryOp.Multiply
        BinaryOp.Divide -> TackyBinaryOp.Divide
        BinaryOp.Remainder -> TackyBinaryOp.Remainder
        BinaryOp.And -> TackyBinaryOp.And
        BinaryOp.Or -> TackyBinaryOp.Or
        BinaryOp.Xor -> TackyBinaryOp.Xor
        BinaryOp.LeftShift -> TackyBinaryOp.LeftShift
        BinaryOp.RightShift -> TackyBinaryOp.RightShift
    }

    fun convert(expression: Expression): Value = when (expression) {
        is Constant -> TackyConstant(expression.value)
        is UnaryExpr -> {
            val src = convert(expression.expression)
            val dst = Var(maketemporary())
            instructions += TackyUnary(convert(expression.op), src, dst)
            dst
        }

        is BinaryExpr -> {
            val v1 = convert(expression.left)
            val v2 = convert(expression.right)
            val dst = Var(maketemporary())
            val tackyOp = convert(expression.operator)
            instructions += TackyBinary(tackyOp, v1, v2, dst)
            dst
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
