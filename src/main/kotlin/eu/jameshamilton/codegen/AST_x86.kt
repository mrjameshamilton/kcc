package eu.jameshamilton.codegen

import eu.jameshamilton.codegen.RegisterName.AX
import eu.jameshamilton.codegen.RegisterName.CX
import eu.jameshamilton.codegen.RegisterName.DX
import eu.jameshamilton.codegen.RegisterName.SI
import eu.jameshamilton.codegen.RegisterSize.BYTE
import eu.jameshamilton.codegen.RegisterSize.LONG
import eu.jameshamilton.codegen.RegisterSize.QUAD
import eu.jameshamilton.codegen.RegisterSize.WORD
import eu.jameshamilton.unreachable

const val STACK_ALIGNMENT_BYTES = 16

data class Program(val items: List<TopLevel>)

sealed class TopLevel
data class FunctionDef(
    val name: String,
    val global: Boolean,
    val defined: Boolean,
    val instructions: List<Instruction>
) : TopLevel()

// TODO: make this a type?
typealias StaticInit = Any

data class StaticVariable(val name: String, val global: Boolean, val alignment: Int, val init: StaticInit) :
    TopLevel() {
    init {
        require(init is Int || init is Long || init is UInt || init is ULong || init is Double)
    }

    val size: Int
        get() = when (this.init) {
            is Int, is UInt -> 4
            is Long, is ULong -> 8
            else -> unreachable("No size for ${this}.")
        }

    val initType: String
        get() = when (this.init) {
            is Int, is UInt -> "long"
            is Long, is ULong -> "quad"
            is Double -> "double"
            else -> unreachable("No size for ${this.init}.")
        }
}

data class StaticConstant(val name: String, val alignment: Int, val init: StaticInit) : TopLevel() {
    init {
        require(init is Double || init is ULong)
    }
}

sealed class Instruction

data class Mov(val type: TypeX86, val src: Operand, val dst: Operand) : Instruction()
data class Movsx(val src: Operand, val dst: Operand) : Instruction()
data class Movzx(val src: Operand, val dst: Operand) : Instruction()
data class Cvttsd2si(val dstType: TypeX86, val src: Operand, val dst: Operand) : Instruction()
data class Cvtsi2sd(val srcType: TypeX86, val src: Operand, val dst: Operand) : Instruction()
data object Ret : Instruction()
data class Unary(val op: UnaryOp, val type: TypeX86, val operand: Operand) : Instruction()
enum class UnaryOp {
    Neg, Not
}

data class Binary(val op: BinaryOp, val type: TypeX86, val src: Operand, val dst: Operand) : Instruction()
enum class BinaryOp {
    Add, Sub, IMul, Mul, And, Or, Xor, ArithmeticLeftShift, ArithmeticRightShift, LogicalRightShift
}

data class Cmp(val type: TypeX86, val src1: Operand, val src2: Operand) : Instruction()
data class IDiv(val type: TypeX86, val operand: Operand) : Instruction()
data class Div(val type: TypeX86, val operand: Operand) : Instruction()
data class DivDouble(val type: TypeX86, val src: Operand, val dst: Operand) : Instruction()
data class Cdq(val type: TypeX86) : Instruction()
data class Jmp(val identifier: String) : Instruction()
data class JmpCC(val conditionCode: ConditionCode, val identifier: String) : Instruction()
data class SetCC(val conditionCode: ConditionCode, val operand: Operand) : Instruction()
data class Label(val identifier: String) : Instruction()
enum class ConditionCode {
    E,  // ZF set
    NE, // ZF not set
    G,  //
    GE, //
    L,  //
    LE, //
    A,  // CF not set and ZF not set
    AE, // CF not set
    B,  // CF set
    BE  // CF set or ZF set
}

data class Push(val operand: Operand) : Instruction()
data class Call(val identifier: String) : Instruction()

sealed class Operand(open val type: TypeX86)
data class Imm(override val type: TypeX86, val value: Any) : Operand(type) {
    init {
        require(value is Int || value is Long || value is UInt || value is ULong)
    }
}

data class Register(val name: RegisterName, val size: RegisterSize) : Operand(Unknown)

// EAX is the full 32-bit value
// AX is the lower 16-bits
// AL is the lower 8 bits
// AH is the bits 8 through 15 (zero-based), the top half of AX
// RAX is the full 64-bits on x86_64
enum class RegisterName {
    AX, CX, DX, DI, SI, R8, R9, R10, R11, SP,
    XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM14, XMM15
}

enum class RegisterSize(val suffix: String) {
    BYTE("b"), WORD("w"), LONG("d"), QUAD("")
}

object RegisterAlias {
    val SP = Register(RegisterName.SP, QUAD)

    val RAX = Register(AX, QUAD)
    val EAX = Register(AX, LONG)
    val AL = Register(AX, BYTE)
    val AH = Register(AX, BYTE)

    val RDX = Register(DX, QUAD)
    val EDX = Register(DX, LONG)

    val RDI = Register(DX, QUAD)
    val EDI = Register(DX, LONG)

    val RSI = Register(SI, QUAD)
    val ESI = Register(SI, LONG)

    val RCX = Register(CX, QUAD)
    val ECX = Register(CX, LONG)

    val XMM0 = Register(RegisterName.XMM0, QUAD)
    val XMM1 = Register(RegisterName.XMM1, QUAD)
    val XMM2 = Register(RegisterName.XMM2, QUAD)
    val XMM3 = Register(RegisterName.XMM3, QUAD)
    val XMM4 = Register(RegisterName.XMM4, QUAD)
    val XMM5 = Register(RegisterName.XMM5, QUAD)
    val XMM6 = Register(RegisterName.XMM6, QUAD)
    val XMM7 = Register(RegisterName.XMM7, QUAD)
    val XMM14 = Register(RegisterName.XMM14, QUAD)
    val XMM15 = Register(RegisterName.XMM15, QUAD)
}

val RegisterName.b: Register
    get() = Register(this, BYTE)
val RegisterName.w: Register
    get() = Register(this, WORD)
val RegisterName.d: Register
    get() = Register(this, LONG)
val RegisterName.q: Register
    get() = Register(this, QUAD)
val RegisterName.sd: Register
    get() = Register(this, QUAD)

fun RegisterName.x(type: TypeX86) = when (type) {
    Longword -> Register(this, LONG)
    Quadword -> Register(this, QUAD)
    Unknown -> unreachable("No size for ${this}.")
    Double_ -> unreachable("Use XMM registers for double types.")
}

val Register.b: Register
    get() = Register(this.name, BYTE)
val Register.d: Register
    get() = Register(this.name, LONG)

typealias Bytes = Int

sealed interface Memory
data class Pseudo(override val type: TypeX86, val identifier: String) : Operand(type)
data class Stack(val position: Int) : Operand(Unknown), Memory

data class Data(override val type: TypeX86, val identifier: String) : Operand(type), Memory
