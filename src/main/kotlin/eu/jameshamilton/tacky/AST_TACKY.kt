package eu.jameshamilton.tacky

import eu.jameshamilton.frontend.FunType
import eu.jameshamilton.frontend.Type


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

data class StaticVariable(val name: String, val global: Boolean, val type: Type, val init: Any) : TopLevel() {
    init {
        require(init is Int || init is Long || init is UInt || init is ULong || init is Double)
    }
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

sealed class Value
data class Constant(val value: Any) : Value() {
    init {
        require(value is Int || value is Long || value is UInt || value is ULong || value is Double)
    }
}

data class Var(val type: Type, val name: String) : Value()

typealias LabelIdentifier = String

data class Copy(val src: Value, val dst: Value) : Instruction()
data class GetAddress(val src: Value, val dst: Value) : Instruction()
data class Load(val src: Value, val dst: Value) : Instruction()
data class Store(val src: Value, val dst: Value) : Instruction()
data class Jump(val target: LabelIdentifier) : Instruction()
data class JumpIfZero(val condition: Value, val target: LabelIdentifier) : Instruction()
data class JumpIfNotZero(val condition: Value, val target: LabelIdentifier) : Instruction()
data class Label(val identifier: LabelIdentifier) : Instruction()