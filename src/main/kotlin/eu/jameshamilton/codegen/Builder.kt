package eu.jameshamilton.codegen

import eu.jameshamilton.codegen.ConditionCode.E
import eu.jameshamilton.codegen.ConditionCode.G
import eu.jameshamilton.codegen.ConditionCode.GE
import eu.jameshamilton.codegen.ConditionCode.L
import eu.jameshamilton.codegen.ConditionCode.LE
import eu.jameshamilton.codegen.ConditionCode.NE

class Builder(var instructions: List<Instruction> = mutableListOf()) {
    fun mov(src: Operand, dst: Operand) {
        instructions += Mov(src, dst)
    }

    fun mov(i: Int, dst: Operand) {
        mov(Imm(i), dst)
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

    fun cmp(src1: Operand, src2: Operand) {
        instructions += Cmp(src1, src2)
    }

    fun cmp(i: Int, src2: Operand) {
        cmp(Imm(i), src2)
    }

    fun cmp(registerName: RegisterName, src2: Operand) {
        cmp(Register(registerName), src2)
    }

    fun cmp(src1: Operand, registerName: RegisterName) {
        cmp(src1, Register(registerName))
    }

    fun setcc(code: ConditionCode, src: Operand) {
        instructions += SetCC(code, src)
    }

    fun sete(src: Operand) {
        setcc(E, src)
    }

    fun setne(src: Operand) {
        setcc(NE, src)
    }

    fun setg(src: Operand) {
        setcc(G, src)
    }

    fun setge(src: Operand) {
        setcc(GE, src)
    }

    fun setl(src: Operand) {
        setcc(L, src)
    }

    fun setle(src: Operand) {
        setcc(LE, src)
    }

    fun jcc(code: ConditionCode, target: String) {
        instructions += JmpCC(code, target)
    }

    fun je(target: String) {
        jcc(E, target)
    }

    fun jne(target: String) {
        jcc(NE, target)
    }

    fun jmp(target: String) {
        instructions += Jmp(target)
    }

    fun label(identifier: String) {
        instructions += Label(identifier)
    }
}

fun buildX86(block: Builder.() -> Unit): List<Instruction> = with(Builder()) {
    block(this)
    return instructions
}