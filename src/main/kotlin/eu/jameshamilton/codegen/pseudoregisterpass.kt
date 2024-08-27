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
import eu.jameshamilton.frontend.check.StaticAttr
import eu.jameshamilton.frontend.check.symbolTable
import kotlin.math.ceil


fun replacePseudoRegisters(program: Program): Program {
    val registers = mutableMapOf<Pseudo, Stack>()

    fun allocate(op: Operand): Operand = when (op) {
        is Pseudo -> if (symbolTable[op.identifier]?.attr is StaticAttr) {
            Data(op.identifier)
        } else {
            registers.computeIfAbsent(op) { Stack(-(registers.size + 1)) }
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
                    mov(src, R10)
                    mov(R10, dst)
                } else {
                    mov(src, dst)
                }
            }

            is Unary -> unary(instruction.op, allocate(instruction.operand))
            Ret -> ret()
            is Binary -> {
                val left = allocate(instruction.src)
                val right = allocate(instruction.dst)
                when (instruction.op) {
                    // These can't have both operands as memory locations.
                    Add, Sub, And, Or, Xor -> if (left is Memory && right is Memory) {
                        mov(left, R10)
                        binary(instruction.op, R10, right)
                    } else {
                        binary(instruction.op, left, right)
                    }

                    Mul -> if (right is Memory) {
                        // imul can't use memory address as its destination.
                        mov(right, R11)
                        imul(left, R11)
                        mov(R11, right)
                    } else {
                        binary(instruction.op, left, right)
                    }

                    LeftShift, RightShift -> if (left is Memory) {
                        mov(left, CX)
                        binary(instruction.op, CX, right)
                        mov(CX, left)
                    } else {
                        binary(instruction.op, left, right)
                    }
                }
            }

            Cdq -> cdq()
            is IDiv -> {
                val operand = allocate(instruction.operand)
                if (operand is Imm) {
                    mov(operand, R10)
                    idiv(R10)
                } else {
                    idiv(operand)
                }
            }

            is Cmp -> {
                val src1 = allocate(instruction.src1)
                val src2 = allocate(instruction.src2)
                if (src1 is Memory && src2 is Memory) {
                    mov(src1, R10)
                    cmp(R10, src2)
                } else if (src2 is Imm) {
                    mov(src2, R11)
                    cmp(src1, R11)
                } else {
                    cmp(src1, src2)
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

            is AllocateStack -> allocate(instruction.i)
            is DeallocateStack -> deallocate(instruction.i)
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
        return FunctionDef(functionDef.name, functionDef.global, prologue + this)
    }

    fun fixup(topLevel: TopLevel): TopLevel = when (topLevel) {
        is FunctionDef -> fixup(topLevel)
        is StaticVariable -> fixup(topLevel)
    }

    return Program(program.items.map { fixup(it) })
}
