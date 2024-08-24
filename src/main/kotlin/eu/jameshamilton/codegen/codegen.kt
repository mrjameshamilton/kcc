package eu.jameshamilton.codegen

import eu.jameshamilton.codegen.BinaryOp.Add
import eu.jameshamilton.codegen.BinaryOp.And
import eu.jameshamilton.codegen.BinaryOp.LeftShift
import eu.jameshamilton.codegen.BinaryOp.Mul
import eu.jameshamilton.codegen.BinaryOp.Or
import eu.jameshamilton.codegen.BinaryOp.RightShift
import eu.jameshamilton.codegen.BinaryOp.Sub
import eu.jameshamilton.codegen.BinaryOp.Xor
import eu.jameshamilton.codegen.RegisterName.AX
import eu.jameshamilton.codegen.RegisterName.CX
import eu.jameshamilton.codegen.RegisterName.DX
import eu.jameshamilton.codegen.RegisterName.R10
import eu.jameshamilton.codegen.RegisterName.R11
import eu.jameshamilton.codegen.Size.BYTE
import eu.jameshamilton.codegen.Size.LONG
import eu.jameshamilton.codegen.Size.QUAD
import eu.jameshamilton.codegen.Size.WORD
import eu.jameshamilton.codegen.UnaryOp.Neg
import eu.jameshamilton.codegen.UnaryOp.Not
import eu.jameshamilton.unreachable
import eu.jameshamilton.codegen.FunctionDef as x86FunctionDef
import eu.jameshamilton.codegen.Program as x86Program

fun emit(x86program: x86Program): String = buildString {
    fun format(operand: Operand): String = when (operand) {
        is Imm -> "$${operand.value}"


        is Register -> when (operand.name) {
            AX, CX, DX -> when (operand.size) {
                BYTE -> "%${operand.name.name.lowercase().first()}l"
                WORD -> "%${operand.name.name.lowercase()}"
                LONG -> "%e${operand.name.name.lowercase()}"
                QUAD -> "%r${operand.name.name.lowercase()}"
            }

            R10, R11 -> "%${operand.name.name.lowercase()}${operand.size.suffix}"
        }

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
                is Cmp -> appendLine("    cmpl ${format(it.src1)}, ${format(it.src2)}")
                is Jmp -> appendLine("    jmp .L${it.identifier}")
                is JmpCC -> appendLine("    j${it.conditionCode.toString().lowercase()} .L${it.identifier}")
                is Label -> appendLine(".L${it.identifier}:")
                is SetCC -> appendLine("    set${it.conditionCode.toString().lowercase()} ${format(it.operand)}")
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

    x86program.functions.forEach {
        emit(it)
    }

    appendLine(""".section .note-GNU-stack,"",@progbits""")
}
