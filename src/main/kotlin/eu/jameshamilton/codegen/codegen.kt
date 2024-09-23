package eu.jameshamilton.codegen

import eu.jameshamilton.codegen.BinaryOp.Add
import eu.jameshamilton.codegen.BinaryOp.And
import eu.jameshamilton.codegen.BinaryOp.ArithmeticLeftShift
import eu.jameshamilton.codegen.BinaryOp.ArithmeticRightShift
import eu.jameshamilton.codegen.BinaryOp.IMul
import eu.jameshamilton.codegen.BinaryOp.LogicalRightShift
import eu.jameshamilton.codegen.BinaryOp.Mul
import eu.jameshamilton.codegen.BinaryOp.Or
import eu.jameshamilton.codegen.BinaryOp.Sub
import eu.jameshamilton.codegen.BinaryOp.Xor
import eu.jameshamilton.codegen.RegisterName.AX
import eu.jameshamilton.codegen.RegisterName.BP
import eu.jameshamilton.codegen.RegisterName.CX
import eu.jameshamilton.codegen.RegisterName.DI
import eu.jameshamilton.codegen.RegisterName.DX
import eu.jameshamilton.codegen.RegisterName.R10
import eu.jameshamilton.codegen.RegisterName.R11
import eu.jameshamilton.codegen.RegisterName.R8
import eu.jameshamilton.codegen.RegisterName.R9
import eu.jameshamilton.codegen.RegisterName.SI
import eu.jameshamilton.codegen.RegisterName.SP
import eu.jameshamilton.codegen.RegisterName.XMM0
import eu.jameshamilton.codegen.RegisterName.XMM1
import eu.jameshamilton.codegen.RegisterName.XMM14
import eu.jameshamilton.codegen.RegisterName.XMM15
import eu.jameshamilton.codegen.RegisterName.XMM2
import eu.jameshamilton.codegen.RegisterName.XMM3
import eu.jameshamilton.codegen.RegisterName.XMM4
import eu.jameshamilton.codegen.RegisterName.XMM5
import eu.jameshamilton.codegen.RegisterName.XMM6
import eu.jameshamilton.codegen.RegisterName.XMM7
import eu.jameshamilton.codegen.RegisterSize.BYTE
import eu.jameshamilton.codegen.RegisterSize.LONG
import eu.jameshamilton.codegen.RegisterSize.QUAD
import eu.jameshamilton.codegen.RegisterSize.WORD
import eu.jameshamilton.codegen.UnaryOp.Neg
import eu.jameshamilton.codegen.UnaryOp.Not
import eu.jameshamilton.frontend.check.CharInit
import eu.jameshamilton.frontend.check.DoubleInit
import eu.jameshamilton.frontend.check.IntInit
import eu.jameshamilton.frontend.check.LongInit
import eu.jameshamilton.frontend.check.PointerInit
import eu.jameshamilton.frontend.check.StaticInit
import eu.jameshamilton.frontend.check.StringInit
import eu.jameshamilton.frontend.check.UCharInit
import eu.jameshamilton.frontend.check.UIntInit
import eu.jameshamilton.frontend.check.ULongInit
import eu.jameshamilton.frontend.check.ZeroInit
import eu.jameshamilton.codegen.FunctionDef as x86FunctionDef
import eu.jameshamilton.codegen.Program as x86Program

fun emit(x86program: x86Program): String = buildString {
    fun format(operand: Operand): String = when (operand) {
        is Imm -> "$${operand.value}"

        is Register -> when (operand.name) {
            SP -> "%rsp"
            BP -> "%rbp"
            AX, CX, DX, DI, SI -> when (operand.size) {
                BYTE -> "%${operand.name.name.lowercase().first()}l"
                WORD -> "%${operand.name.name.lowercase()}"
                LONG -> "%e${operand.name.name.lowercase()}"
                QUAD -> "%r${operand.name.name.lowercase()}"
            }

            R8, R9, R10, R11 -> "%${operand.name.name.lowercase()}${operand.size.suffix}"

            XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM14, XMM15 -> "%${operand.name.name.lowercase()}"
        }

        is Mem -> if (operand.position == 0) "(${format(operand.base)})" else "${operand.position}(${format(operand.base)})"
        is Data -> if (operand.isConstant) ".L${operand.identifier}(%rip)" else "${operand.identifier}(%rip)"
        is Indexed -> "(${format(operand.base)}, ${format(operand.index)}, ${operand.scale})"
        // Only for debugging purposes: these are not valid assembly.
        is Pseudo -> "%${operand.identifier}"
        is PseudoMem -> "%${operand.identifier}@${operand.offset}"
    }

    fun format(op: UnaryOp, type: TypeX86): String = when (op) {
        Neg -> "neg${type.suffix}"
        Not -> "not${type.suffix}"
    }

    fun format(op: BinaryOp, type: TypeX86): String = when (op) {
        Add -> "add${type.suffix}"
        Sub -> "sub${type.suffix}"
        IMul -> "imul${type.suffix}"
        Mul -> "mul${type.suffix}"
        And -> "and${type.suffix}"
        Or -> "or${type.suffix}"
        Xor -> "xor${if (type is Double_) "pd" else type.suffix}"
        ArithmeticLeftShift -> "sal${type.suffix}"
        ArithmeticRightShift -> "sar${type.suffix}"
        LogicalRightShift -> "shr${type.suffix}"
    }

    fun emit(functionName: String, functionId: Int, instructions: List<Instruction>) {
        fun label(identifier: String): String = ".L${functionName}${functionId}_${identifier}"

        instructions.forEach {
            when (it) {
                is Mov -> appendLine("    mov${it.type.suffix} ${format(it.src)}, ${format(it.dst)}")
                is Movsx -> appendLine("    movslq ${format(it.src)}, ${format(it.dst)}")
                is Movzx -> appendLine("    movz ${format(it.src)}, ${format(it.dst)}")
                is Lea -> appendLine("    leaq ${format(it.src)}, ${format(it.dst)}")
                Ret -> appendLine(
                    """
                    |    movq %rbp, %rsp
                    |    popq %rbp 
                    |    ret
                """.trimMargin()
                )

                is Unary -> appendLine("    ${format(it.op, it.type)} ${format(it.operand)}")
                is Binary -> appendLine("    ${format(it.op, it.type)} ${format(it.src)}, ${format(it.dst)}")
                is IDiv -> appendLine("    idiv${it.type.suffix} ${format(it.operand)}")
                is Div -> appendLine("    div${it.type.suffix} ${format(it.operand)}")
                is DivDouble -> appendLine("    div${it.type.suffix} ${format(it.src)}, ${format(it.dst)}")
                is Cdq -> appendLine("    c${if (it.type is Longword) "dq" else "qo"}")
                is Cmp -> {
                    when {
                        it.type is Double_ -> appendLine("    comi${it.type.suffix} ${format(it.src1)}, ${format(it.src2)}")
                        else -> appendLine("    cmp${it.type.suffix} ${format(it.src1)}, ${format(it.src2)}")
                    }
                }

                is Jmp -> appendLine("    jmp ${label(it.identifier)}")
                is JmpCC -> appendLine("    j${it.conditionCode.toString().lowercase()} ${label(it.identifier)}")
                is Label -> appendLine("${label(it.identifier)}:")
                is SetCC -> appendLine("    set${it.conditionCode.toString().lowercase()} ${format(it.operand)}")
                is Call -> appendLine("    call ${it.identifier}@PLT")
                is Push -> appendLine("    pushq ${format(it.operand)}")
                is Cvtsi2sd -> appendLine("    cvtsi2sd${it.srcType.suffix} ${format(it.src)}, ${format(it.dst)}")
                is Cvttsd2si -> appendLine("    cvttsd2si${it.dstType.suffix} ${format(it.src)}, ${format(it.dst)}")
            }
        }
    }

    fun emit(staticInit: StaticInit<*>) {
        fun type(initType: StaticInit<*>) = when (initType) {
            is IntInit, is UIntInit -> "long"
            is LongInit, is ULongInit -> "quad"
            is DoubleInit -> "double"
            is ZeroInit -> "zero"
            is CharInit -> TODO()
            is UCharInit -> TODO()
            is PointerInit -> TODO()
            is StringInit -> TODO()
        }

        when (staticInit) {
            is ZeroInit -> appendLine("    .zero ${staticInit.bytes}")
            else -> appendLine("    .${type(staticInit)} ${staticInit.value}")
        }
    }

    fun emit(staticVariable: StaticVariable) {
        if (staticVariable.global) {
            appendLine("    .globl ${staticVariable.name}")
        }

        if (staticVariable.init.singleOrNull() is ZeroInit) {
            appendLine(
                """
            |    .bss
            |    .align ${staticVariable.alignment}
            |${staticVariable.name}:
            """.trimMargin()
            )
            emit(staticVariable.init.single())
        } else {
            appendLine(
                """
            |    .data
            |    .align ${staticVariable.alignment}
            |${staticVariable.name}:
            """.trimMargin()
            )
            staticVariable.init.forEach(::emit)
        }
        appendLine()
    }

    fun emit(staticConstant: StaticConstant) {
        appendLine(
            """    .align ${staticConstant.alignment}
               |.L${staticConstant.name}:
            """.trimMargin()
        )

        when (staticConstant.init.value) {
            is Double -> appendLine("    .quad ${(staticConstant.init.value as Double).toBits()} # ${staticConstant.init.value}")
            is ULong -> appendLine("    .double ${staticConstant.init.value}")
            else -> emit(staticConstant.init)
        }
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

    appendLine(".section .rodata")
    x86program.items.filterIsInstance<StaticConstant>().forEach {
        emit(it)
    }

    appendLine(""".section .note-GNU-stack,"",@progbits""")
}
