package eu.jameshamilton.codegen

data class Program(val functionDef: FunctionDef)

data class FunctionDef(val name: String, val instructions: List<Instruction>)

sealed class Instruction

data class Mov(val src: Operand, val dst: Operand) : Instruction()
data object Ret : Instruction()

sealed class Operand
data class Imm(val value: Int) : Operand()
data object Register : Operand()
