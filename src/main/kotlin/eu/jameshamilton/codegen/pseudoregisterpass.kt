package eu.jameshamilton.codegen

import eu.jameshamilton.codegen.BinaryOp.Add
import eu.jameshamilton.codegen.BinaryOp.And
import eu.jameshamilton.codegen.BinaryOp.ArithmeticLeftShift
import eu.jameshamilton.codegen.BinaryOp.ArithmeticRightShift
import eu.jameshamilton.codegen.BinaryOp.LogicalRightShift
import eu.jameshamilton.codegen.BinaryOp.Mul
import eu.jameshamilton.codegen.BinaryOp.Or
import eu.jameshamilton.codegen.BinaryOp.Sub
import eu.jameshamilton.codegen.BinaryOp.Xor
import eu.jameshamilton.codegen.RegisterName.CX
import eu.jameshamilton.codegen.RegisterName.R10
import eu.jameshamilton.codegen.RegisterName.R11
import eu.jameshamilton.unreachable
import kotlin.math.abs
import kotlin.math.ceil
import kotlin.math.floor


fun replacePseudoRegisters(program: Program): Program {
    val registers = LinkedHashMap<Pseudo, Stack>()

    fun allocate(op: Operand): Operand = when (op) {
        is Pseudo -> if (op.isStatic) {
            Data(op.identifier)
        } else {
            // Always allocate 8 bytes to ensure correct alignment
            // even if the type requires less space than 8.
            registers.computeIfAbsent(op) {
                Stack(-((registers.size + 1) * Quadword.size))
            }
        }

        else -> op
    }

    fun fixup(instruction: Instruction): List<Instruction> = buildX86 {
        when (instruction) {
            is Mov -> {
                val src = allocate(instruction.src)
                val dst = allocate(instruction.dst)

                when {
                    src is Imm && src.type == Quadword && dst is Memory -> {
                        // cannot mov quadwords directly into memory.
                        mov(instruction.type, src, R10.q)
                        mov(instruction.type, R10.q, dst)
                    }

                    src is Memory && dst is Memory -> {
                        // mov can't have both operands as memory locations.
                        mov(instruction.type, src, R10.x(instruction.type))
                        mov(instruction.type, R10.x(instruction.type), dst)
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
                    src is Imm && dst is Memory -> {
                        movl(src, R10.d)
                        movsx(R10.d, R11.q)
                        movq(R11.q, dst)
                    }

                    src is Imm -> {
                        // mov src into long register, which
                        // results in the upper 32bits set to zero.
                        movl(src, R10.d)
                        movsx(R10.q, dst)
                    }

                    dst is Memory -> {
                        movsx(src, R10.q)
                        movq(R10.q, dst)
                    }
                }
            }

            is Movzx -> {
                val src = allocate(instruction.src)
                val dst = allocate(instruction.dst)

                when {
                    dst is Register -> {
                        movl(src, dst)
                    }

                    dst is Memory -> {
                        movl(src, R11.d)
                        movq(R11.q, dst)
                    }
                }
            }

            is Unary -> unary(instruction.type, instruction.op, allocate(instruction.operand))
            Ret -> ret()
            is Binary -> {
                val left = allocate(instruction.src)
                val right = allocate(instruction.dst)
                when (instruction.op) {
                    // These can't have both operands as memory locations.
                    Add, Sub, And, Or, Xor -> when {
                        left is Imm && left.type is Quadword || right is Imm && right.type is Quadword -> {
                            if (left.type is Quadword && right.type is Quadword) {
                                mov(instruction.type, left, R10.q)
                                mov(instruction.type, right, R11.q)
                                binary(instruction.type, instruction.op, R10.q, R11.q)
                            } else {
                                mov(instruction.type, left, R10.q)
                                binary(instruction.type, instruction.op, R10.q, right)
                            }
                        }

                        left is Memory && right is Memory -> {
                            mov(instruction.type, left, R10.x(instruction.type))
                            binary(instruction.type, instruction.op, R10.x(instruction.type), right)
                        }

                        else -> {
                            binary(instruction.type, instruction.op, left, right)
                        }
                    }

                    Mul -> when {
                        left is Imm && left.type == Quadword && right is Memory -> {
                            // imul can't use memory address as its destination.
                            // can't have Quadword immediate source
                            mov(instruction.type, left, R10.q)
                            mov(instruction.type, right, R11.x(instruction.type))
                            imul(instruction.type, R10.q, R11.x(instruction.type))
                            mov(instruction.type, R11.x(instruction.type), right)
                        }

                        left is Imm && left.type == Quadword -> {
                            mov(instruction.type, left, R10.q)
                            binary(instruction.type, instruction.op, R10.q, right)
                        }

                        right is Memory -> {
                            // imul can't use memory address as its destination.
                            mov(instruction.type, right, R11.x(instruction.type))
                            imul(instruction.type, left, R11.x(instruction.type))
                            mov(instruction.type, R11.x(instruction.type), right)
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

            is Cmp -> {
                val src1 = allocate(instruction.src1)
                val src2 = allocate(instruction.src2)
                when {
                    src1 is Imm && src1.type == Quadword -> when {
                        // cmp cannot have a quadword immediate value
                        // as either operand.
                        src1.type == Quadword && src2.type == Quadword -> {
                            mov(instruction.type, src1, R10.q)
                            mov(instruction.type, src2, R11.q)
                            cmp(instruction.type, R10.q, R11.q)
                        }

                        src1.type == Quadword -> {
                            mov(instruction.type, src1, R10.q)
                            cmp(instruction.type, R10.q, src2)
                        }

                        src2.type == Quadword -> {
                            mov(instruction.type, src2, R10.q)
                            cmp(instruction.type, src1, R10.q)
                        }
                    }

                    src2 is Imm -> {
                        // cmp cannot have an immediate second operand.
                        mov(instruction.type, src2, R10.x(src2.type))
                        cmp(instruction.type, src1, R10.x(src2.type))
                    }

                    src1 is Memory && src2 is Memory -> {
                        // cmp cannot have both operands as memory.
                        mov(instruction.type, src1, R10.x(instruction.type))
                        cmp(instruction.type, R10.x(instruction.type), src2)
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
                        is Int -> true
                        is UInt -> operand.value < 2147483647u
                        is Long, is ULong -> false
                        else -> unreachable("unknown type ${operand.value.javaClass}")
                    }
                }

                if (operand is Imm && !immFitsInSignedInteger) {
                    movq(operand, R10.q)
                    push(R10.q)
                } else {
                    push(operand)
                }
            }
        }
    }


    fun fixup(staticVariable: StaticVariable): StaticVariable =
        staticVariable

    fun fixup(functionDef: FunctionDef): FunctionDef = with(functionDef.instructions.flatMap(::fixup)) {
        val prologue = buildX86 {
            val maxStack = registers.lastEntry()?.value?.position ?: 0
            allocate(abs(maxStack).roundUpToNearestMultiple(STACK_ALIGNMENT_BYTES))
        }
        return FunctionDef(functionDef.name, functionDef.global, functionDef.defined, prologue + this)
    }

    fun fixup(topLevel: TopLevel): TopLevel = when (topLevel) {
        is FunctionDef -> fixup(topLevel)
        is StaticVariable -> fixup(topLevel)
    }

    return Program(program.items.map { fixup(it) })
}

fun Int.roundUpToNearestMultiple(n: Int) = (ceil(this / n.toFloat()) * n).toInt()
fun Int.roundDownToNearestMultiple(n: Int) = (floor(this / n.toFloat()) * n).toInt()
