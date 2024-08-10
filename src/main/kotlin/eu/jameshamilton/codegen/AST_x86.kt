package eu.jameshamilton.codegen

data class Program(val functionDef: FunctionDef)

data class FunctionDef(val name: String, val instructions: List<Instruction>)

sealed class Instruction

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

data class Cmp(val src1: Operand, val src2: Operand) : Instruction()
data class IDiv(val operand: Operand) : Instruction()
data object Cdq : Instruction()
data class Jmp(val identifier: String) : Instruction()
data class JmpCC(val conditionCode: ConditionCode, val identifier: String) : Instruction()
data class SetCC(val conditionCode: ConditionCode, val operand: Operand) : Instruction()
data class Label(val identifier: String) : Instruction()
enum class ConditionCode {
    E, NE, G, GE, L, LE
}

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
