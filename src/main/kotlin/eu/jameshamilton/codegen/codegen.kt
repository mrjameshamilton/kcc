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
import eu.jameshamilton.codegen.RegisterName.DI
import eu.jameshamilton.codegen.RegisterName.DX
import eu.jameshamilton.codegen.RegisterName.R10
import eu.jameshamilton.codegen.RegisterName.R11
import eu.jameshamilton.codegen.RegisterName.R8
import eu.jameshamilton.codegen.RegisterName.R9
import eu.jameshamilton.codegen.RegisterName.SI
import eu.jameshamilton.codegen.RegisterName.SP
import eu.jameshamilton.codegen.RegisterSize.BYTE
import eu.jameshamilton.codegen.RegisterSize.LONG
import eu.jameshamilton.codegen.RegisterSize.QUAD
import eu.jameshamilton.codegen.RegisterSize.WORD
import eu.jameshamilton.codegen.UnaryOp.Neg
import eu.jameshamilton.codegen.UnaryOp.Not
import eu.jameshamilton.unreachable
import eu.jameshamilton.codegen.FunctionDef as x86FunctionDef
import eu.jameshamilton.codegen.Program as x86Program

fun emit(x86program: x86Program): String = buildString {
    fun format(operand: Operand): String = when (operand) {
        is Imm -> "$${operand.value}"

        is Register -> when (operand.name) {
            SP -> "%rsp"
            AX, CX, DX, DI, SI -> when (operand.size) {
                BYTE -> "%${operand.name.name.lowercase().first()}l"
                WORD -> "%${operand.name.name.lowercase()}"
                LONG -> "%e${operand.name.name.lowercase()}"
                QUAD -> "%r${operand.name.name.lowercase()}"
            }

            R8, R9, R10, R11 -> "%${operand.name.name.lowercase()}${operand.size.suffix}"
        }

        is Pseudo -> unreachable("pseudo instruction not emitted")
        is Stack -> "${operand.position}(%rbp)"
        is Data -> "${operand.identifier}(%rip)"
    }

    fun format(op: UnaryOp): String = when (op) {
        Neg -> "neg"
        Not -> "not"
    }

    fun format(op: BinaryOp): String = when (op) {
        Add -> "add"
        Sub -> "sub"
        Mul -> "imul"
        And -> "and"
        Or -> "or"
        Xor -> "xor"
        LeftShift -> "sal"
        RightShift -> "sar"
    }

    fun emit(functionName: String, functionId: Int, instructions: List<Instruction>) {
        fun label(identifier: String): String = ".L${functionName}${functionId}_${identifier}"

        instructions.forEach {
            when (it) {
                is Mov -> appendLine("    mov${it.type.suffix} ${format(it.src)}, ${format(it.dst)}")
                is Movsx -> appendLine("    movslq ${format(it.src)}, ${format(it.dst)}")
                Ret -> appendLine(
                    """
                    |    movq %rbp, %rsp
                    |    popq %rbp 
                    |    ret
                """.trimMargin()
                )

                is Unary -> appendLine("    ${format(it.op)}${it.type.suffix} ${format(it.operand)}")
                is Binary -> appendLine("    ${format(it.op)}${it.type.suffix} ${format(it.src)}, ${format(it.dst)}")
                is IDiv -> appendLine("    idiv${it.type.suffix} ${format(it.operand)}")
                is Cdq -> appendLine("    c${if (it.type is Longword) "dq" else "qo"}")
                is Cmp -> appendLine("    cmp${it.type.suffix} ${format(it.src1)}, ${format(it.src2)}")
                is Jmp -> appendLine("    jmp ${label(it.identifier)}")
                is JmpCC -> appendLine("    j${it.conditionCode.toString().lowercase()} ${label(it.identifier)}")
                is Label -> appendLine("${label(it.identifier)}:")
                is SetCC -> appendLine("    set${it.conditionCode.toString().lowercase()} ${format(it.operand)}")
                is Call -> appendLine("    call ${it.identifier}@PLT")
                is Push -> appendLine("    pushq ${format(it.operand)}")
            }
        }
    }

    fun emit(staticVariable: StaticVariable) {
        if (staticVariable.global) {
            appendLine("    .globl ${staticVariable.name}")
        }

        if (staticVariable.init == 0) {
            appendLine(
                """
            |    .bss
            |    .align ${staticVariable.alignment}
            |${staticVariable.name}:
            |    .zero ${staticVariable.size}
            """.trimMargin()
            )
        } else {
            appendLine(
                """
            |    .data
            |    .align ${staticVariable.alignment}
            |${staticVariable.name}:
            |    .${staticVariable.initType} ${staticVariable.init}  
            """.trimMargin()
            )
        }
        appendLine()
    }

    fun emit(id: Int, functionDef: x86FunctionDef) {
        if (functionDef.global) {
            appendLine("    .globl ${functionDef.name}")
            appendLine("    .type ${functionDef.name},@function")
        }
        appendLine(
            """
            |    .text
            |${functionDef.name}:
            |    pushq %rbp
            |    movq  %rsp, %rbp
        """.trimMargin()
        )

        emit(functionDef.name, id, functionDef.instructions)

        appendLine()
    }

    x86program.items.filterIsInstance<StaticVariable>().forEach {
        emit(it)
    }

    x86program.items.filterIsInstance<x86FunctionDef>().forEachIndexed { id, function ->
        emit(id, function)
    }

    appendLine(""".section .note-GNU-stack,"",@progbits""")
}
