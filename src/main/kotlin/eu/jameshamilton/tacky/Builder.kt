package eu.jameshamilton.tacky

import eu.jameshamilton.frontend.IntType

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
        instructions += Copy(Constant(IntType, i), dst)
        return dst
    }

    fun store(src: Value, dst: Value): Value {
        instructions += Store(src, dst)
        return dst
    }

    fun load(src: Value, dst: Value): Value {
        instructions += Load(src, dst)
        return dst
    }

    fun getaddress(src: Value, dst: Value): Value {
        instructions += GetAddress(src, dst)
        return dst
    }

    fun binaryOp(binaryOp: BinaryOp, src1: Value, src2: Value, dst: Value): Value {
        instructions += Binary(binaryOp, src1, src2, dst)
        return dst
    }

    fun sub(src1: Value, src2: Value, dst: Value): Value {
        return binaryOp(BinaryOp.Subtract, src1, src2, dst)
    }

    fun add(src1: Value, src2: Value, dst: Value): Value {
        return binaryOp(BinaryOp.Add, src1, src2, dst)
    }

    fun div(src1: Value, src2: Value, dst: Value): Value {
        return binaryOp(BinaryOp.Divide, src1, src2, dst)
    }

    fun equal(src1: Value, src2: Value, dst: Value): Value {
        return binaryOp(BinaryOp.Equal, src1, src2, dst)
    }

    fun increment(src: Value, amount: Value): Value {
        instructions += Binary(BinaryOp.Add, src, amount, src)
        return src
    }

    fun unaryOp(unaryOp: UnaryOp, src: Value, dst: Value): Value {
        instructions += Unary(unaryOp, src, dst)
        return dst
    }

    fun neg(src: Value, dst: Value): Value {
        return unaryOp(UnaryOp.Negate, src, dst)
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

    fun dtoi(src: Value, dst: Value): Value {
        instructions += DoubleToInt(src, dst)
        return dst
    }

    fun itod(src: Value, dst: Value): Value {
        instructions += IntToDouble(src, dst)
        return dst
    }

    fun dtoui(src: Value, dst: Value): Value {
        instructions += DoubleToUInt(src, dst)
        return dst
    }

    fun uitod(src: Value, dst: Value): Value {
        instructions += UIntToDouble(src, dst)
        return dst
    }

    fun copytooffset(src: Value, dst: Var, offset: Int) {
        instructions += CopyToOffset(src, dst, offset)
    }

    fun addptr(ptr: Value, index: Value, scale: Int, dst: Value): Value {
        instructions += AddPtr(ptr, index, scale, dst)
        return dst
    }

    fun nop(): Value = Constant(IntType, 0)
}

fun buildTacky(instructions: MutableList<Instruction>, block: Builder.() -> ExprResult): ExprResult =
    with(Builder(instructions)) {
        return block(this)
    }
