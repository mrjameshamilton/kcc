package eu.jameshamilton.codegen

import eu.jameshamilton.frontend.Constant
import eu.jameshamilton.frontend.Expression
import eu.jameshamilton.frontend.FunctionDef
import eu.jameshamilton.frontend.Program
import eu.jameshamilton.frontend.ReturnStatement
import eu.jameshamilton.frontend.Statement
import eu.jameshamilton.frontend.UnaryExpr
import eu.jameshamilton.codegen.FunctionDef as x86FunctionDef
import eu.jameshamilton.codegen.Program as x86Program

fun generate(program: Program): x86Program = x86Program(generate(program.function))

fun generate(functionDef: FunctionDef): x86FunctionDef {
    fun convert(expression: Expression): Operand = when (expression) {
        is Constant -> Imm(expression.value)
        is UnaryExpr -> TODO()
    }

    fun convert(statement: Statement): List<Instruction> = when (statement) {
        is ReturnStatement -> listOf(Mov(convert(statement.value), Register), Ret)
    }

    fun convert(statements: List<Statement>): List<Instruction> = statements.flatMap(::convert)

    return x86FunctionDef(functionDef.name.lexeme, convert(functionDef.body))
}

fun emit(x86program: x86Program): String = buildString {
    fun format(operand: Operand): String = when (operand) {
        is Imm -> "$${operand.value}"
        Register -> "%eax"
    }

    fun emit(instructions: List<Instruction>) {
        instructions.forEach {
            when (it) {
                is Mov -> appendLine("    movl ${format(it.src)}, ${format(it.dst)}")
                Ret -> appendLine("    ret")
            }
        }
    }

    fun emit(functionDef: x86FunctionDef) {
        appendLine("""
            |    .globl ${functionDef.name}
            |${functionDef.name}:
        """.trimMargin())

        emit(functionDef.instructions)
    }

    emit(x86program.functionDef)

    appendLine(""".section .note-GNU-stack,"",@progbits""")
}