package eu.jameshamilton.codegen

import eu.jameshamilton.codegen.UnaryOp.Neg
import eu.jameshamilton.codegen.UnaryOp.Not
import eu.jameshamilton.codegen.FunctionDef as x86FunctionDef
import eu.jameshamilton.codegen.Program as x86Program

fun emit(x86program: x86Program): String = buildString {
    fun format(operand: Operand): String = when (operand) {
        is Imm -> "$${operand.value}"
        is Register -> operand.name.toString()
        is Pseudo -> TODO()
        is Stack -> "${operand.loc}(%rbp)"
    }

    fun format(op: UnaryOp): String = when (op) {
        Neg -> "negl"
        Not -> "notl"
    }

    fun emit(instructions: List<Instruction>) {
        instructions.forEach {
            when (it) {
                is Mov -> appendLine("    movl ${format(it.src)}, ${format(it.dst)}")
                Ret -> appendLine(
                    """
                    |    movq %rbp, %rsp
                    |    popq %rbp 
                    |    ret
                """.trimMargin()
                )

                is Unary -> appendLine("    ${format(it.op)} ${format(it.operand)}")
                is AllocateStack -> appendLine("    subq $${it.i}, %rsp")
                is MultiInstruction -> emit(it.instructions)
            }
        }
    }

    fun emit(functionDef: x86FunctionDef) {
        appendLine(
            """
            |    .globl ${functionDef.name}
            |${functionDef.name}:
            |    pushq %rbp
            |    movq  %rsp, %rbp
        """.trimMargin()
        )

        emit(functionDef.instructions)
    }

    emit(x86program.functionDef)

    appendLine(""".section .note-GNU-stack,"",@progbits""")
}
