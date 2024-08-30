package eu.jameshamilton.codegen

const val STACK_ALIGNMENT_BYTES = 16

data class Program(val items: List<TopLevel>)

sealed class TopLevel
data class FunctionDef(
    val name: String,
    val global: Boolean,
    val defined: Boolean,
    val instructions: List<Instruction>
) : TopLevel()

data class StaticVariable(val name: String, val global: Boolean, val alignment: Int, val init: Any) : TopLevel() {
    init {
        require(init is Int || init is Long)
    }
}

sealed class Instruction

data class Mov(val type: TypeX86, val src: Operand, val dst: Operand) : Instruction()
data class Movsx(val src: Operand, val dst: Operand) : Instruction()
data object Ret : Instruction()
data class Unary(val op: UnaryOp, val type: TypeX86, val operand: Operand) : Instruction()
enum class UnaryOp {
    Neg, Not
}

data class Binary(val op: BinaryOp, val type: TypeX86, val src: Operand, val dst: Operand) : Instruction()
enum class BinaryOp {
    Add, Sub, Mul, And, Or, Xor, LeftShift, RightShift
}

data class Cmp(val type: TypeX86, val src1: Operand, val src2: Operand) : Instruction()
data class IDiv(val type: TypeX86, val operand: Operand) : Instruction()
data class Cdq(val type: TypeX86) : Instruction()
data class Jmp(val identifier: String) : Instruction()
data class JmpCC(val conditionCode: ConditionCode, val identifier: String) : Instruction()
data class SetCC(val conditionCode: ConditionCode, val operand: Operand) : Instruction()
data class Label(val identifier: String) : Instruction()
enum class ConditionCode {
    E, NE, G, GE, L, LE
}

data class Push(val operand: Operand) : Instruction()
data class Call(val identifier: String) : Instruction()

sealed class Operand(open val type: TypeX86)
data class Imm(override val type: TypeX86, val value: Any) : Operand(type) {
    init {
        require(value is Int || value is Long)
    }
}

data class Register(val name: RegisterName, val size: Size = Size.LONG) : Operand(Unknown)

// EAX is the full 32-bit value
// AX is the lower 16-bits
// AL is the lower 8 bits
// AH is the bits 8 through 15 (zero-based), the top half of AX
// RAX is the full 64-bits on x86_64
enum class RegisterName {
    AX, CX, DX, DI, SI, R8, R9, R10, R11, SP;
}

enum class Size(val suffix: String) {
    BYTE("b"), WORD("w"), LONG("d"), QUAD("")
}

typealias Bytes = Int

sealed interface Memory
data class Pseudo(override val type: TypeX86, val identifier: String) : Operand(type)
data class Stack(val position: Int, val size: Bytes = 4) : Operand(Unknown), Memory {
    val loc: Int = position * size
}

data class Data(val identifier: String) : Operand(Unknown), Memory
