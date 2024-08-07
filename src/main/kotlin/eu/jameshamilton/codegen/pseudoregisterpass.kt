package eu.jameshamilton.codegen

import eu.jameshamilton.codegen.RegisterName.R10

fun replacePseudoRegisters(program: Program): Program {
    val registers = mutableMapOf<Pseudo, Stack>()

    fun convert(op: Operand): Operand = when (op) {
        is Pseudo -> registers.computeIfAbsent(op) { Stack((registers.size + 1) * -4) }
        else -> op
    }

    fun convert(instruction: Instruction): Instruction = when (instruction) {
        is Mov -> {
            val src = convert(instruction.src)
            val dst = convert(instruction.dst)

            if (src is Stack && dst is Stack) {
                MultiInstruction(listOf(Mov(src, Register(R10)), Mov(Register(R10), dst)))
            } else {
                Mov(src, dst)
            }
        }
        is Unary -> Unary(instruction.op, convert(instruction.operand))
        is AllocateStack -> AllocateStack(instruction.i)
        is MultiInstruction -> MultiInstruction(instruction.instructions.map(::convert))
        Ret -> instruction
    }

    fun convert(functionDef: FunctionDef): FunctionDef {
        val instructions = functionDef.instructions.map(::convert)
        return FunctionDef(functionDef.name, listOf(AllocateStack(registers.size * 4)) + instructions)
    }

    return Program(convert(program.functionDef))
}
