package eu.jameshamilton.tacky


data class Program(val functionDef: FunctionDef)

data class FunctionDef(val name: String, val instructions: List<Instruction>)

sealed class Instruction

data class TackyReturn(val value: Value) : Instruction()
data class Unary(val op: UnaryOp, val src: Value, val dst: Value) : Instruction()
enum class UnaryOp {
    Complement, Negate
}

sealed class Value
data class Constant(val value: Int) : Value()
data class Var(val name: String) : Value()
