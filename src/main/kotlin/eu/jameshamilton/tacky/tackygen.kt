package eu.jameshamilton.tacky

import eu.jameshamilton.frontend.Constant
import eu.jameshamilton.frontend.Expression
import eu.jameshamilton.frontend.FunctionDef
import eu.jameshamilton.frontend.Program
import eu.jameshamilton.frontend.ReturnStatement
import eu.jameshamilton.frontend.Statement
import eu.jameshamilton.frontend.UnaryExpr
import eu.jameshamilton.frontend.UnaryOp
import eu.jameshamilton.tacky.Constant as TackyConstant
import eu.jameshamilton.tacky.FunctionDef as TackyFunctionDef
import eu.jameshamilton.tacky.Program as TackyProgram
import eu.jameshamilton.tacky.Unary as TackyUnary

fun convert(program: Program): TackyProgram = TackyProgram(convert(program.function))

fun convert(program: FunctionDef): TackyFunctionDef {

    val instructions = mutableListOf<Instruction>()

    fun convert(op: UnaryOp): eu.jameshamilton.tacky.UnaryOp = when (op) {
        UnaryOp.Complement -> eu.jameshamilton.tacky.UnaryOp.Complement
        UnaryOp.Negate -> eu.jameshamilton.tacky.UnaryOp.Negate
    }

    var count = 0
    fun maketemporary(): String = "tmp.${count++}"

    fun convert(expression: Expression): Value = when (expression) {
        is Constant -> TackyConstant(expression.value)
        is UnaryExpr -> {
            val src = convert(expression.expression)
            val dst = Var(maketemporary())
            instructions += TackyUnary(convert(expression.op), src, dst)
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
