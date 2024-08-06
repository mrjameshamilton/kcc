package eu.jameshamilton.codegen

import eu.jameshamilton.Constant
import eu.jameshamilton.Expression
import eu.jameshamilton.FunctionDef
import eu.jameshamilton.Program
import eu.jameshamilton.ReturnStatement
import eu.jameshamilton.Statement
import eu.jameshamilton.UnaryExpr
import eu.jameshamilton.codegen.FunctionDef as x86FunctionDef
import eu.jameshamilton.codegen.Program as x86Program

fun generate(program: Program): x86Program {
    return x86Program(generate(program.function))
}

fun generate(functionDef: FunctionDef): x86FunctionDef {
    fun convert(expression: Expression): Operand {
        return when (expression) {
            is Constant -> Imm(expression.value)
            is UnaryExpr -> TODO()
        }
    }

    fun convert(statement: Statement): List<Instruction> {
        return when (statement) {
            is ReturnStatement -> listOf(Mov(convert(statement.value), Register), Ret)
        }
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

    fun emit(functionDef: eu.jameshamilton.codegen.FunctionDef) {
        appendLine("""
            |    .globl ${functionDef.name}
            |${functionDef.name}:
        """.trimMargin())

        emit(functionDef.instructions)
    }

    emit(x86program.functionDef)

    appendLine(""".section .note-GNU-stack,"",@progbits""")
}