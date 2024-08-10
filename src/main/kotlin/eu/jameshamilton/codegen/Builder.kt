package eu.jameshamilton.codegen

class Builder(var instructions: List<Instruction> = mutableListOf()) {
    fun mov(src: Operand, dst: Operand) {
        instructions += Mov(src, dst)
    }

    fun mov(registerName: RegisterName, dst: Operand) {
        mov(Register(registerName), dst)
    }

    fun mov(src: Operand, dst: RegisterName) {
        mov(src, Register(dst))
    }

    fun cdq() {
        instructions += Cdq
    }

    fun idiv(registerName: RegisterName) {
        idiv(Register(registerName))
    }

    fun idiv(operand: Operand) {
        instructions += IDiv(operand)
    }

    fun ret() {
        instructions += Ret
    }

    fun binary(binaryOp: BinaryOp, src: Operand, dst: Operand) {
        instructions += Binary(binaryOp, src, dst)
    }

    fun binary(binaryOp: BinaryOp, src: RegisterName, dst: Operand) {
        instructions += Binary(binaryOp, Register(src), dst)
    }

    fun unary(unaryOp: UnaryOp, src: Operand) {
        instructions += Unary(unaryOp, src)
    }

    fun not(src: Operand) {
        unary(UnaryOp.Not, src)
    }

    fun neg(src: Operand) {
        unary(UnaryOp.Neg, src)
    }

    fun add(src: Operand, dst: Operand) {
        binary(BinaryOp.Add, src, dst)
    }

    fun sub(src: Operand, dst: Operand) {
        binary(BinaryOp.Sub, src, dst)
    }

    fun imul(src: Operand, dst: Operand) {
        binary(BinaryOp.Mul, src, dst)
    }

    fun imul(src: Operand, dst: RegisterName) {
        binary(BinaryOp.Mul, src, Register(dst))
    }

    fun and(src: Operand, dst: Operand) {
        binary(BinaryOp.And, src, dst)
    }

    fun or(src: Operand, dst: Operand) {
        binary(BinaryOp.Or, src, dst)
    }

    fun xor(src: Operand, dst: Operand) {
        binary(BinaryOp.Xor, src, dst)
    }

    fun sal(src: Operand, dst: Operand) {
        binary(BinaryOp.LeftShift, src, dst)
    }

    fun sar(src: Operand, dst: Operand) {
        binary(BinaryOp.RightShift, src, dst)
    }
}

fun buildX86(block: Builder.() -> Unit): List<Instruction> = with(Builder()) {
    block(this)
    return instructions
}