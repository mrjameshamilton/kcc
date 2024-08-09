package eu.jameshamilton.codegen

data class Program(val functionDef: FunctionDef)

data class FunctionDef(val name: String, val instructions: List<Instruction>)

sealed class Instruction

data class MultiInstruction(val instructions: List<Instruction>) : Instruction()
data class Mov(val src: Operand, val dst: Operand) : Instruction()
data object Ret : Instruction()
data class Unary(val op: UnaryOp, val operand: Operand) : Instruction()
enum class UnaryOp {
    Neg, Not
}

data class Binary(val op: BinaryOp, val src: Operand, val dst: Operand) : Instruction()
enum class BinaryOp {
    Add, Sub, Mul, And, Or, Xor, LeftShift, RightShift
}

data class IDiv(val operand: Operand) : Instruction()
data object Cdq : Instruction()
data class AllocateStack(val i: Int) : Instruction()

sealed class Operand
data class Imm(val value: Int) : Operand()
data class Register(val name: RegisterName) : Operand()
enum class RegisterName(private val s: String) {
    AX("%eax"), R10("%r10d"), DX("%edx"), R11("%r11d"), CX("%ecx");

    override fun toString(): String {
        return s
    }
}

data class Pseudo(val identifier: String) : Operand()
data class Stack(val loc: Int) : Operand()

operator fun Instruction.plus(other: Instruction) = listOf(this, other)
operator fun Instruction.plus(other: List<Instruction>) = listOf(this) + other
