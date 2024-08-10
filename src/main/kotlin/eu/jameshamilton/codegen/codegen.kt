package eu.jameshamilton.codegen

import eu.jameshamilton.codegen.BinaryOp.Add
import eu.jameshamilton.codegen.BinaryOp.And
import eu.jameshamilton.codegen.BinaryOp.LeftShift
import eu.jameshamilton.codegen.BinaryOp.Mul
import eu.jameshamilton.codegen.BinaryOp.Or
import eu.jameshamilton.codegen.BinaryOp.RightShift
import eu.jameshamilton.codegen.BinaryOp.Sub
import eu.jameshamilton.codegen.BinaryOp.Xor
import eu.jameshamilton.codegen.UnaryOp.Neg
import eu.jameshamilton.codegen.UnaryOp.Not
import eu.jameshamilton.unreachable
import eu.jameshamilton.codegen.FunctionDef as x86FunctionDef
import eu.jameshamilton.codegen.Program as x86Program

fun emit(x86program: x86Program): String = buildString {
    fun format(operand: Operand): String = when (operand) {
        is Imm -> "$${operand.value}"
        is Register -> operand.name.toString()
        is Pseudo -> unreachable("pseudo instruction not emitted")
        is Stack -> "${operand.loc}(%rbp)"
    }

    fun format(op: UnaryOp): String = when (op) {
        Neg -> "negl"
        Not -> "notl"
    }

    fun format(op: BinaryOp): String = when (op) {
        Add -> "addl"
        Sub -> "subl"
        Mul -> "imull"
        And -> "andl"
        Or -> "orl"
        Xor -> "xorl"
        LeftShift -> "sall"
        RightShift -> "sarl"
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
                is Binary -> appendLine("    ${format(it.op)} ${format(it.src)}, ${format(it.dst)}")
                is IDiv -> appendLine("    idivl ${format(it.operand)}")
                Cdq -> appendLine("    cdq")
                is Cmp -> TODO()
                is Jmp -> TODO()
                is JmpCC -> TODO()
                is Label -> TODO()
                is SetCC -> TODO()
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
