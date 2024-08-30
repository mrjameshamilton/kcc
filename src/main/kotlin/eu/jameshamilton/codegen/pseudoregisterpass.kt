package eu.jameshamilton.codegen

import eu.jameshamilton.codegen.BinaryOp.Add
import eu.jameshamilton.codegen.BinaryOp.And
import eu.jameshamilton.codegen.BinaryOp.LeftShift
import eu.jameshamilton.codegen.BinaryOp.Mul
import eu.jameshamilton.codegen.BinaryOp.Or
import eu.jameshamilton.codegen.BinaryOp.RightShift
import eu.jameshamilton.codegen.BinaryOp.Sub
import eu.jameshamilton.codegen.BinaryOp.Xor
import eu.jameshamilton.codegen.RegisterName.CX
import eu.jameshamilton.codegen.RegisterName.R10
import eu.jameshamilton.codegen.RegisterName.R11
import eu.jameshamilton.unreachable
import kotlin.math.ceil


fun replacePseudoRegisters(program: Program): Program {
    val registers = mutableMapOf<Pseudo, Stack>()

    fun allocate(op: Operand): Operand = when (op) {
        is Pseudo -> if (op.isStatic) {
            Data(op.identifier)
        } else {
            val size = when (op.type) {
                Longword -> 4
                Quadword -> 8
                Unknown -> unreachable("invalid type: ${op.type}")
            }
            registers.computeIfAbsent(op) { Stack(-(registers.size + 1), size) }
        }

        else -> op
    }

    fun fixup(instruction: Instruction): List<Instruction> = buildX86 {
        when (instruction) {
            is Mov -> {
                val src = allocate(instruction.src)
                val dst = allocate(instruction.dst)

                // mov can't have both operands as memory locations.
                if (src is Memory && dst is Memory) {
                    mov(instruction.type, src, R10)
                    mov(instruction.type, R10, dst)
                } else {
                    mov(instruction.type, src, dst)
                }
            }

            is Movsx -> {
                val src = allocate(instruction.src)
                val dst = allocate(instruction.dst)

                when {
                    src is Imm && dst is Memory -> {
                        mov(Longword, src, R10)
                        movsx(R10, R11)
                        mov(Quadword, R11, dst)
                    }

                    src is Imm -> {
                        mov(Longword, src, R10)
                        movsx(R10, dst)
                    }

                    dst is Memory -> {
                        movsx(src, R10)
                        mov(Longword, R10, dst)
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
                    Add, Sub, And, Or, Xor -> if (left is Memory && right is Memory) {
                        mov(instruction.type, left, R10)
                        binary(instruction.type, instruction.op, R10, right)
                    } else {
                        binary(instruction.type, instruction.op, left, right)
                    }

                    Mul -> if (right is Memory) {
                        // imul can't use memory address as its destination.
                        mov(instruction.type, right, R11)
                        imul(instruction.type, left, R11)
                        mov(instruction.type, R11, right)
                    } else {
                        binary(instruction.type, instruction.op, left, right)
                    }

                    LeftShift, RightShift -> if (left is Memory) {
                        mov(instruction.type, left, CX)
                        binary(instruction.type, instruction.op, CX, right)
                        mov(instruction.type, CX, left)
                    } else {
                        binary(instruction.type, instruction.op, left, right)
                    }
                }
            }

            is Cdq -> cdq(instruction.type)
            is IDiv -> {
                val operand = allocate(instruction.operand)
                if (operand is Imm) {
                    mov(instruction.type, operand, R10)
                    idiv(instruction.type, R10)
                } else {
                    idiv(instruction.type, operand)
                }
            }

            is Cmp -> {
                val src1 = allocate(instruction.src1)
                val src2 = allocate(instruction.src2)
                if (src1 is Memory && src2 is Memory) {
                    mov(instruction.type, src1, R10)
                    cmp(instruction.type, R10, src2)
                } else if (src2 is Imm) {
                    mov(instruction.type, src2, R11)
                    cmp(instruction.type, src1, R11)
                } else {
                    cmp(instruction.type, src1, src2)
                }
            }

            is Jmp -> jmp(instruction.identifier)
            is JmpCC -> jcc(instruction.conditionCode, instruction.identifier)
            is Label -> label(instruction.identifier)
            is SetCC -> {
                val operand = allocate(instruction.operand)
                // setcc uses 1 byte registers.
                if (operand is Register && operand.size != Size.BYTE) {
                    setcc(instruction.conditionCode, Register(operand.name, Size.BYTE))
                } else {
                    setcc(instruction.conditionCode, operand)
                }
            }

            is Call -> call(instruction.identifier)
            is Push -> push(allocate(instruction.operand))
        }
    }


    fun fixup(staticVariable: StaticVariable): StaticVariable =
        staticVariable

    fun fixup(functionDef: FunctionDef): FunctionDef = with(functionDef.instructions.flatMap(::fixup)) {
        val prologue = buildX86 {
            val total = registers.map { it.value.size }.sum()
            val roundedStackSize = (ceil(total / STACK_ALIGNMENT_BYTES.toFloat()) * STACK_ALIGNMENT_BYTES).toInt()
            allocate(roundedStackSize)
        }
        return FunctionDef(functionDef.name, functionDef.global, functionDef.defined, prologue + this)
    }

    fun fixup(topLevel: TopLevel): TopLevel = when (topLevel) {
        is FunctionDef -> fixup(topLevel)
        is StaticVariable -> fixup(topLevel)
    }

    return Program(program.items.map { fixup(it) })
}
