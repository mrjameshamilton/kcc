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
import eu.jameshamilton.codegen.RegisterAlias.BP
import eu.jameshamilton.codegen.RegisterAlias.SP
import eu.jameshamilton.codegen.RegisterAlias.XMM14
import eu.jameshamilton.codegen.RegisterAlias.XMM15
import eu.jameshamilton.codegen.RegisterName.CX
import eu.jameshamilton.codegen.RegisterName.R10
import eu.jameshamilton.codegen.RegisterName.R11
import eu.jameshamilton.unreachable
import kotlin.math.abs
import kotlin.math.ceil
import kotlin.math.floor

private typealias Registers = LinkedHashMap<String, Mem>

private val Registers.max: Long
    get() = lastEntry()?.value?.position ?: 0L

fun replacePseudoRegisters(program: Program): Program {
    val registers = Registers()

    fun allocate(op: Operand): Operand = when (op) {
        is Pseudo -> when {
            op.isStatic -> Data(op.type, op.identifier)
            else ->
                // Always allocate 8 bytes to ensure correct alignment
                // even if the type requires less space than 8.
                registers.computeIfAbsent(op.identifier) {
                    val position = (registers.max - op.type.size).roundDownToNearestMultiple(8)
                    Mem(op.type, BP, position)
                }
        }

        is PseudoMem -> when {
            op.isStatic && op.offset == 0L -> {
                // Should not happen that offset > 0, because we only use
                // pseudomem operands with nonzero offsets to initialize arrays with
                // automatic storage duration, not to access arrays with static storage duration.
                Data(op.type, op.identifier)
            }

            op.isStatic -> unreachable("PseudoMem static offset must be zero")
            op.type is ByteArray && op.identifier !in registers -> {
                // First time allocating an array, allocate space for the whole array.
                val position =
                    (registers.max - op.type.size).roundDownToNearestMultiple((op.type as ByteArray).alignment)
                registers.computeIfAbsent(op.identifier) {
                    Mem(op.type, BP, position)
                }
            }

            op.type is ByteArray && op.identifier in registers -> {
                // Subsequent times, return offsets into the existing array allocation.
                val array = registers[op.identifier]!!
                val position = (array.position + op.offset)//.roundDownToNearestMultiple(alignment)
                Mem(op.type, array.base, position)
            }

            else -> unreachable("Invalid type for PseudoMem: $op")
        }

        else -> op
    }

    fun fixup(instruction: Instruction): List<Instruction> = buildX86 {
        when (instruction) {
            is Mov -> {
                val src = allocate(instruction.src)
                val dst = allocate(instruction.dst)

                when {
                    src is Imm && dst.type is Byte_ && src.type in setOf(Longword, Quadword) -> {
                        val truncated = when (src.value) {
                            // Truncate immediates to byte if they don't fit.
                            is Int -> Imm(Byte_, src.value.toByte())
                            is UInt -> Imm(Byte_, src.value.toUByte())
                            is Long -> Imm(Byte_, src.value.toByte())
                            is ULong -> Imm(Byte_, src.value.toUByte())
                            else -> unreachable("Invalid type for Imm: $src")
                        }
                        movb(truncated, dst)
                    }

                    src is Imm && src.type == Quadword && dst is Memory -> {
                        // cannot mov quadwords directly into memory.
                        mov(instruction.type, src, R10.q)
                        mov(instruction.type, R10.q, dst)
                    }

                    src is Memory && dst is Memory -> {
                        // mov can't have both operands as memory locations.
                        if (instruction.type == Double_) {
                            mov(instruction.type, src, XMM14)
                            mov(instruction.type, XMM14, dst)
                        } else {
                            mov(instruction.type, src, R10.x(instruction.type))
                            mov(instruction.type, R10.x(instruction.type), dst)
                        }
                    }

                    src is Imm && src.type is Double_ && dst is Memory -> {
                        movsd(src, XMM15)
                        movsd(XMM15, dst)
                    }

                    src is Imm && src.type is Double_ -> {
                        movsd(src, dst)
                    }

                    else -> {
                        mov(instruction.type, src, dst)
                    }
                }
            }

            is Movsx -> {
                val src = allocate(instruction.src)
                val dst = allocate(instruction.dst)

                when {
                    src is Imm -> when {
                        dst is Memory -> {
                            movl(src, R10.d)
                            movsx(Longword, Quadword, R10.d, R11.q)
                            movq(R11.q, dst)
                        }

                        else -> {
                            // mov src into long register, which
                            // results in the upper 32bits set to zero.
                            movl(src, R10.d)
                            movsx(Quadword, dst.type, R10.q, dst)
                        }
                    }

                    dst is Memory -> {
                        movsx(src.type, Quadword, src, R10.q)
                        movq(R10.q, dst)
                    }

                    else -> {
                        movsx(src.type, dst.type, src, dst)
                    }
                }
            }

            is Movzx -> {
                val src = allocate(instruction.src)
                val dst = allocate(instruction.dst)

                when {
                    src.type is Byte_ && (src is Imm || dst !is Register) -> {
                        movb(src, R10.b)
                        movzxbq(R10.b, R11.q)
                        movq(R11.q, dst)
                    }

                    dst is Memory -> {
                        movl(src, R11.d)
                        movq(R11.q, dst)
                    }

                    else -> {
                        movzx(src.type, dst.type, src, dst)
                    }
                }
            }

            is Lea -> {
                val src = allocate(instruction.src)
                when (val dst = allocate(instruction.dst)) {
                    !is Register -> {
                        lea(src, R10.q)
                        movq(R10.q, dst)
                    }

                    else -> lea(src, dst)
                }
            }

            is Unary -> unary(instruction.type, instruction.op, allocate(instruction.operand))
            Ret -> ret()
            is Binary -> {
                val left = allocate(instruction.src)
                val right = allocate(instruction.dst)
                when (instruction.op) {
                    Add, Sub, And, Or, Xor -> when {
                        left is Imm && left.type is Quadword -> {
                            if (right is Imm && right.type is Quadword) {
                                mov(instruction.type, left, R10.q)
                                mov(instruction.type, right, R11.q)
                                binary(instruction.type, instruction.op, R10.q, R11.q)
                            } else {
                                mov(instruction.type, left, R10.q)
                                binary(instruction.type, instruction.op, R10.q, right)
                            }
                        }

                        // These can't have both operands as memory locations.
                        left is Memory && right is Memory -> {
                            if (instruction.type is Double_) {
                                movsd(left, XMM14)
                                movsd(right, XMM15)
                                binary(instruction.type, instruction.op, XMM14, XMM15)
                                movsd(XMM15, right)
                            } else {
                                mov(instruction.type, left, R10.x(instruction.type))
                                binary(instruction.type, instruction.op, R10.x(instruction.type), right)
                            }
                        }

                        else -> {
                            binary(instruction.type, instruction.op, left, right)
                        }
                    }

                    IMul, Mul -> when {
                        left is Imm && left.type == Quadword && right is Memory -> {
                            // imul can't use memory address as its destination.
                            // can't have Quadword immediate source
                            mov(instruction.type, left, R10.q)
                            mov(instruction.type, right, R11.x(instruction.type))
                            mul(instruction.type, R10.q, R11.x(instruction.type))
                            mov(instruction.type, R11.x(instruction.type), right)
                        }

                        left is Imm && left.type == Quadword -> {
                            // can't have Quadword immediate source
                            mov(instruction.type, left, R10.q)
                            binary(instruction.type, instruction.op, R10.q, right)
                        }

                        right is Memory -> {
                            // imul can't use memory address as its destination.
                            if (instruction.type is Double_) {
                                movsd(right, XMM14)
                                mulsd(left, XMM14)
                                movsd(XMM14, right)
                            } else {
                                mov(instruction.type, right, R11.x(instruction.type))
                                mul(instruction.type, left, R11.x(instruction.type))
                                mov(instruction.type, R11.x(instruction.type), right)
                            }
                        }

                        else -> {
                            binary(instruction.type, instruction.op, left, right)
                        }
                    }

                    ArithmeticLeftShift, ArithmeticRightShift, LogicalRightShift -> {
                        val (count, dst) = Pair(left, right)
                        // count can only be the CX register or immediate.
                        when {
                            count is Imm -> {
                                binary(instruction.type, instruction.op, count, dst)
                            }

                            else -> {
                                mov(instruction.type, count, CX.x(instruction.type))
                                binary(instruction.type, instruction.op, CX.x(instruction.type), dst)
                            }
                        }
                    }
                }
            }

            is Cdq -> cdq(instruction.type)
            is IDiv -> {
                when (val operand = allocate(instruction.operand)) {
                    is Imm -> {
                        mov(instruction.type, operand, R10.x(instruction.type))
                        idiv(instruction.type, R10.x(instruction.type))
                    }

                    else -> {
                        idiv(instruction.type, operand)
                    }
                }
            }

            is Div -> {
                when (val operand = allocate(instruction.operand)) {
                    is Imm -> {
                        mov(instruction.type, operand, R10.x(instruction.type))
                        div(instruction.type, R10.x(instruction.type))
                    }

                    else -> {
                        div(instruction.type, operand)
                    }
                }
            }

            is DivDouble -> {
                val src = allocate(instruction.src)
                val dst = allocate(instruction.dst)
                divdouble(Double_, src, dst)
            }

            is Cmp -> {
                val src1 = allocate(instruction.src1)
                val src2 = allocate(instruction.src2)
                when {
                    src1 is Imm && src1.type is Quadword || src2 is Imm && src2.type is Quadword -> {
                        // cmp cannot have a quadword immediate value as either operand.
                        mov(instruction.type, src1, R10.q)
                        mov(instruction.type, src2, R11.q)
                        cmp(instruction.type, R10.q, R11.q)
                    }

                    (src1 is Imm && src1.type is Double_) || (src2 is Imm && src2.type is Double_) -> {
                        movsd(src1, XMM14)
                        movsd(src2, XMM15)
                        comisd(XMM14, XMM15)
                    }

                    src2 is Imm -> {
                        // cmp cannot have an immediate second operand.
                        mov(instruction.type, src2, R10.x(src2.type))
                        cmp(instruction.type, src1, R10.x(src2.type))
                    }

                    src2 is Memory && instruction.type is Double_ -> {
                        movsd(src2, XMM15)
                        comisd(src1, XMM15)
                    }

                    src1 is Memory && src2 is Memory -> {
                        // cmp cannot have both operands as memory.
                        if (instruction.type == Double_) {
                            movsd(src1, XMM14)
                            movsd(src2, XMM15)
                            comisd(XMM14, XMM15)
                        } else {
                            mov(instruction.type, src1, R10.x(instruction.type))
                            cmp(instruction.type, R10.x(instruction.type), src2)
                        }
                    }

                    else -> {
                        cmp(instruction.type, src1, src2)
                    }
                }
            }

            is Jmp -> jmp(instruction.identifier)
            is JmpCC -> jcc(instruction.conditionCode, instruction.identifier)
            is Label -> label(instruction.identifier)
            is SetCC -> {
                val operand = allocate(instruction.operand)
                if (operand is Register) {
                    // setcc uses 1 byte registers.
                    setcc(instruction.conditionCode, operand.b)
                } else {
                    setcc(instruction.conditionCode, operand)
                }
            }

            is Call -> call(instruction.identifier)
            is Push -> {
                val operand = allocate(instruction.operand)

                // Can only use immediate values in pushq, addq, imulq, subq, cmpq
                // if it can be represented as a signed 32-bit integer because
                // the instructions sign extend their operands from 32-bit to 64-bit.
                val immFitsInSignedInteger by lazy {
                    operand is Imm && when (operand.value) {
                        is Int, is Byte -> true
                        is UInt -> operand.value < 2147483647u
                        is Long, is ULong -> false
                        else -> unreachable("unknown type ${operand.value.javaClass}")
                    }
                }

                when {
                    operand is Register && operand.type is Double_ -> {
                        subq(Imm(Quadword, 8), SP)
                        movsd(operand, Mem(Quadword, SP, 0))
                    }

                    operand is Imm && !immFitsInSignedInteger -> {
                        movq(operand, R10.q)
                        push(R10.q)
                    }

                    else -> {
                        push(operand)
                    }
                }
            }

            is Cvtsi2sd -> {
                val src = allocate(instruction.src)
                val dst = allocate(instruction.dst)

                when {
                    src is Imm && dst is Memory -> {
                        mov(instruction.srcType, src, R10.x(instruction.srcType))
                        cvtsi2sd(instruction.srcType, R10.x(instruction.srcType), XMM14)
                        mov(Double_, XMM14, dst)
                    }

                    dst is Memory -> {
                        cvtsi2sd(instruction.srcType, src, XMM14)
                        mov(Double_, XMM14, dst)
                    }

                    else -> cvttsd2si(instruction.srcType, src, dst)
                }
            }

            is Cvttsd2si -> {
                val src = allocate(instruction.src)
                val dst = allocate(instruction.dst)

                when {
                    dst is Memory -> {
                        cvttsd2si(instruction.dstType, src, R10.x(instruction.dstType))
                        mov(instruction.dstType, R10.x(instruction.dstType), dst)
                    }

                    else -> cvttsd2si(instruction.dstType, src, dst)
                }
            }
        }
    }

    fun fixup(staticVariable: StaticVariable): StaticVariable =
        staticVariable

    fun fixup(staticConstant: StaticConstant): StaticConstant =
        staticConstant

    fun fixup(functionDef: FunctionDef): FunctionDef {
        registers.clear()
        with(functionDef.instructions.flatMap(::fixup)) {
            val prologue = buildX86 {
                allocate(abs(registers.max.toLong()).roundUpToNearestMultiple(STACK_ALIGNMENT_BYTES))
            }
            return FunctionDef(functionDef.name, functionDef.global, functionDef.defined, prologue + this)
        }
    }

    fun fixup(topLevel: TopLevel): TopLevel = when (topLevel) {
        is FunctionDef -> fixup(topLevel)
        is StaticVariable -> fixup(topLevel)
        is StaticConstant -> fixup(topLevel)
    }

    return Program(program.items.map { fixup(it) })
}

fun Long.roundUpToNearestMultiple(n: Long) = (ceil(this / n.toFloat()) * n).toLong()
fun Long.roundDownToNearestMultiple(n: Long) = (floor(this / n.toFloat()) * n).toLong()
