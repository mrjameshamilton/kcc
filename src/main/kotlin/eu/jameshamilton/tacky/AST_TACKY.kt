package eu.jameshamilton.tacky


data class Program(val items: List<TopLevel>)

sealed class TopLevel

data class FunctionDef(
    val name: String,
    val global: Boolean,
    val parameters: List<String>,
    val instructions: List<Instruction>
) : TopLevel()

data class StaticVariable(val name: String, val global: Boolean, val init: Int) : TopLevel()

sealed class Instruction
data class FunctionCall(val name: String, val arguments: List<Value>, val dst: Value) : Instruction()

data class TackyReturn(val value: Value) : Instruction()
data class Unary(val op: UnaryOp, val src: Value, val dst: Value) : Instruction()
enum class UnaryOp {
    Complement, Negate, Not
}

data class Binary(val op: BinaryOp, val src1: Value, val src2: Value, val dst: Value) : Instruction()
enum class BinaryOp {
    Add, Subtract, Multiply, Divide, Remainder,
    And, Or, Xor, LeftShift, RightShift,
    Equal, NotEqual, LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual
}

sealed class Value
data class Constant(val value: Int) : Value()
data class Var(val name: String) : Value()

typealias LabelIdentifier = String

data class Copy(val src: Value, val dst: Value) : Instruction()
data class Jump(val target: LabelIdentifier) : Instruction()
data class JumpIfZero(val condition: Value, val target: LabelIdentifier) : Instruction()
data class JumpIfNotZero(val condition: Value, val target: LabelIdentifier) : Instruction()
data class Label(val identifier: LabelIdentifier) : Instruction()