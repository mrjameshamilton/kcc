package eu.jameshamilton.tacky

class Builder(private val instructions: MutableList<Instruction> = mutableListOf()) {
    fun jump(target: String): Value {
        instructions += Jump(target)
        return nop()
    }

    fun jumpIfZero(condition: Value, target: String) {
        instructions += JumpIfZero(condition, target)
    }

    fun jumpIfNotZero(condition: Value, target: String) {
        instructions += JumpIfNotZero(condition, target)
    }

    fun label(label: LabelIdentifier): Value {
        instructions += Label(label)
        return nop()
    }

    fun copy(src: Value, dst: Value): Value {
        instructions += Copy(src, dst)
        return dst
    }

    fun copy(i: Int, dst: Value): Value {
        instructions += Copy(Constant(i), dst)
        return dst
    }

    fun binaryOp(binaryOp: BinaryOp, src1: Value, src2: Value, dst: Value): Value {
        instructions += Binary(binaryOp, src1, src2, dst)
        return dst
    }

    fun equal(src1: Value, src2: Value, dst: Value): Value {
        return binaryOp(BinaryOp.Equal, src1, src2, dst)
    }

    fun increment(src: Value, amount: Int = 1): Value {
        return increment(src, Constant(amount))
    }

    fun increment(src: Value, amount: Value): Value {
        instructions += Binary(BinaryOp.Add, src, amount, src)
        return src
    }

    fun unaryOp(unaryOp: UnaryOp, src: Value, dst: Value): Value {
        instructions += Unary(unaryOp, src, dst)
        return dst
    }

    fun ret(value: Value) {
        instructions += Return(value)
    }

    fun call(identifier: String, arguments: List<Value>, result: Value): Value {
        instructions += FunctionCall(identifier, arguments, result)
        return result
    }

    fun signextend(src: Value, dst: Value): Value {
        instructions += SignExtend(src, dst)
        return dst
    }

    fun zeroextend(src: Value, dst: Value): Value {
        instructions += ZeroExtend(src, dst)
        return dst
    }

    fun truncate(src: Value, dst: Value): Value {
        instructions += Truncate(src, dst)
        return dst
    }

    fun nop(): Value = Constant(0)
}

fun buildTacky(instructions: MutableList<Instruction>, block: Builder.() -> Value): Value =
    with(Builder(instructions)) {
        return block(this)
    }
