package eu.jameshamilton.tacky

import eu.jameshamilton.frontend.ArrayType
import eu.jameshamilton.frontend.FunType
import eu.jameshamilton.frontend.Type
import eu.jameshamilton.frontend.check.StaticInit


data class Program(val items: List<TopLevel>)

sealed class TopLevel

data class FunctionDef(
    val type: FunType,
    val name: String,
    val global: Boolean,
    val defined: Boolean,
    val parameterNames: List<String>,
    val instructions: List<Instruction>
) : TopLevel() {
    val parameters by lazy { type.paramsTypes.zip(parameterNames) }
}

data class StaticVariable(val name: String, val global: Boolean, val type: Type, val init: List<StaticInit<*>>) :
    TopLevel() {
    constructor(name: String, global: Boolean, type: Type, init: StaticInit<*>) : this(name, global, type, listOf(init))
}

sealed class Instruction
data class FunctionCall(val name: String, val arguments: List<Value>, val dst: Value) : Instruction()

data class Return(val value: Value) : Instruction()
data class SignExtend(val src: Value, val dst: Value) : Instruction()
data class ZeroExtend(val src: Value, val dst: Value) : Instruction()
data class Truncate(val src: Value, val dst: Value) : Instruction()
data class DoubleToInt(val src: Value, val dst: Value) : Instruction()
data class DoubleToUInt(val src: Value, val dst: Value) : Instruction()
data class IntToDouble(val src: Value, val dst: Value) : Instruction()
data class UIntToDouble(val src: Value, val dst: Value) : Instruction()
data class Unary(val op: UnaryOp, val src: Value, val dst: Value) : Instruction()
enum class UnaryOp {
    Complement, Negate, Not
}

data class Binary(val op: BinaryOp, val src1: Value, val src2: Value, val dst: Value) : Instruction()
enum class BinaryOp {
    Add, Subtract, Multiply, Divide, Remainder,
    And, Or, Xor, LeftShift, RightShift, LogicalRightShift,
    Equal, NotEqual, LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual
}

sealed class Value(open val type: Type)
data class Constant(override val type: Type, val value: Any) : Value(type) {
    init {
        require(value is Byte || value is UByte || value is Int || value is UInt || value is Long || value is ULong || value is Double)
    }
}

data class Var(override val type: Type, val name: String) : Value(type)

typealias LabelIdentifier = String

data class Copy(val src: Value, val dst: Value) : Instruction()
data class GetAddress(val src: Value, val dst: Value) : Instruction()
data class Load(val ptr: Value, val dst: Value) : Instruction()
data class Store(val src: Value, val ptr: Value) : Instruction()
data class AddPtr(val ptr: Value, val index: Value, val scale: Int, val dst: Value) : Instruction()
data class CopyToOffset(val src: Value, val dst: Var, val offset: Int) : Instruction() {
    init {
        require(dst.type is ArrayType)
    }
}

data class Jump(val target: LabelIdentifier) : Instruction()
data class JumpIfZero(val condition: Value, val target: LabelIdentifier) : Instruction()
data class JumpIfNotZero(val condition: Value, val target: LabelIdentifier) : Instruction()
data class Label(val identifier: LabelIdentifier) : Instruction()