package eu.jameshamilton.codegen

import eu.jameshamilton.codegen.BinaryOp.Add
import eu.jameshamilton.codegen.BinaryOp.Mul
import eu.jameshamilton.codegen.BinaryOp.Sub
import eu.jameshamilton.codegen.RegisterName.R10
import eu.jameshamilton.codegen.RegisterName.R11

fun replacePseudoRegisters(program: Program): Program {
    val registers = mutableMapOf<Pseudo, Stack>()

    fun allocate(op: Operand): Operand = when (op) {
        is Pseudo -> registers.computeIfAbsent(op) { Stack((registers.size + 1) * -4) }
        else -> op
    }

    fun fixup(instruction: Instruction): Instruction = when (instruction) {
        is Mov -> {
            val src = allocate(instruction.src)
            val dst = allocate(instruction.dst)

            // movl can't have both operands constant.
            if (src is Stack && dst is Stack) {
                MultiInstruction(Mov(src, Register(R10)) + Mov(Register(R10), dst))
            } else {
                Mov(src, dst)
            }
        }

        is Unary -> Unary(instruction.op, allocate(instruction.operand))
        is AllocateStack -> AllocateStack(instruction.i)
        is MultiInstruction -> MultiInstruction(instruction.instructions.map(::fixup))
        Ret -> Ret
        is Binary -> {
            val left = allocate(instruction.src)
            val right = allocate(instruction.dst)
            when (instruction.op) {
                // add, sub can't have both operands constant.
                Add, Sub -> if (left is Stack && right is Stack) {
                    MultiInstruction(Mov(left, Register(R10)) + Binary(instruction.op, Register(R10), right))
                } else {
                    Binary(instruction.op, left, right)
                }

                Mul -> if (right is Stack) {
                    // imul can't use memory address as its destination.
                    MultiInstruction(
                        Mov(right, Register(R11)) +
                                Binary(instruction.op, left, Register(R11)) +
                                Mov(Register(R11), right)
                    )
                } else {
                    Binary(instruction.op, left, right)
                }
            }
        }

        Cdq -> Cdq
        is IDiv -> {
            val operand = allocate(instruction.operand)
            if (operand is Imm) {
                MultiInstruction(Mov(operand, Register(R10)) + IDiv(Register(R10)))
            } else {
                IDiv(operand)
            }
        }
    }

    fun fixup(functionDef: FunctionDef): FunctionDef = with (functionDef.instructions.map(::fixup)) {
        return FunctionDef(functionDef.name, AllocateStack(registers.size * 4) + this)
    }

    return Program(fixup(program.functionDef))
}
