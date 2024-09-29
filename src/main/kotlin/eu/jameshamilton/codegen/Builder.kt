package eu.jameshamilton.codegen

import eu.jameshamilton.codegen.ConditionCode.A
import eu.jameshamilton.codegen.ConditionCode.AE
import eu.jameshamilton.codegen.ConditionCode.B
import eu.jameshamilton.codegen.ConditionCode.BE
import eu.jameshamilton.codegen.ConditionCode.E
import eu.jameshamilton.codegen.ConditionCode.G
import eu.jameshamilton.codegen.ConditionCode.GE
import eu.jameshamilton.codegen.ConditionCode.L
import eu.jameshamilton.codegen.ConditionCode.LE
import eu.jameshamilton.codegen.ConditionCode.NE
import eu.jameshamilton.codegen.ConditionCode.P
import eu.jameshamilton.codegen.RegisterAlias.SP

class Builder(var instructions: List<Instruction> = mutableListOf()) {
    fun mov(type: TypeX86, src: Operand, dst: Operand): Builder {
        instructions += Mov(type, src, dst)
        return this
    }

    fun movb(src: Operand, dst: Operand): Builder {
        return mov(Byte_, src, dst)
    }

    fun movl(src: Operand, dst: Operand): Builder {
        return mov(Longword, src, dst)
    }

    fun movq(src: Operand, dst: Operand): Builder {
        return mov(Quadword, src, dst)
    }

    fun movq(i: Int, dst: Operand): Builder {
        return movq(Imm(Quadword, i), dst)
    }

    fun movsd(src: Operand, dst: Operand): Builder {
        return mov(Double_, src, dst)
    }

    fun mov(type: TypeX86, i: Int, dst: Operand): Builder {
        return mov(type, Imm(type, i), dst)
    }

    fun movsx(srcType: TypeX86, dstType: TypeX86, src: Operand, dst: Operand): Builder {
        instructions += Movsx(srcType, dstType, src, dst)
        return this
    }

    fun movsxbl(src: Operand, dst: Operand): Builder {
        return movsx(Byte_, Longword, src, dst)
    }

    fun movzx(srcType: TypeX86, dstType: TypeX86, src: Operand, dst: Operand): Builder {
        instructions += Movzx(srcType, dstType, src, dst)
        return this
    }

    fun movzxbl(src: Operand, dst: Operand): Builder {
        return movzx(Byte_, Longword, src, dst)
    }

    fun movzxbq(src: Operand, dst: Operand): Builder {
        return movzx(Byte_, Quadword, src, dst)
    }

    fun cdq(type: TypeX86): Builder {
        instructions += Cdq(type)
        return this
    }

    fun idiv(type: TypeX86, operand: Operand): Builder {
        instructions += IDiv(type, operand)
        return this
    }

    fun div(type: TypeX86, operand: Operand): Builder {
        instructions += Div(type, operand)
        return this
    }

    fun divdouble(type: TypeX86, operand: Operand, dst: Operand): Builder {
        instructions += DivDouble(type, operand, dst)
        return this
    }

    fun ret(): Builder {
        instructions += Ret
        return this
    }

    fun binary(type: TypeX86, binaryOp: BinaryOp, src: Operand, dst: Operand): Builder {
        instructions += Binary(binaryOp, type, src, dst)
        return this
    }

    fun unary(type: TypeX86, unaryOp: UnaryOp, src: Operand): Builder {
        instructions += Unary(unaryOp, type, src)
        return this
    }

    fun not(type: TypeX86, src: Operand): Builder {
        return unary(type, UnaryOp.Not, src)
    }

    fun neg(type: TypeX86, src: Operand): Builder {
        return unary(type, UnaryOp.Neg, src)
    }

    fun add(type: TypeX86, src: Operand, dst: Operand): Builder {
        return binary(type, BinaryOp.Add, src, dst)
    }

    fun addsd(src: Operand, dst: Operand): Builder {
        return binary(Double_, BinaryOp.Add, src, dst)
    }

    fun addq(src: Operand, dst: Operand): Builder {
        return add(Quadword, src, dst)
    }

    fun sub(type: TypeX86, src: Operand, dst: Operand): Builder {
        return binary(type, BinaryOp.Sub, src, dst)
    }

    fun subq(src: Operand, dst: Operand): Builder {
        return sub(Quadword, src, dst)
    }

    fun subsd(src: Operand, dst: Operand): Builder {
        return sub(Double_, src, dst)
    }

    fun mul(type: TypeX86, src: Operand, dst: Operand): Builder {
        return binary(type, BinaryOp.IMul, src, dst)
    }

    fun mulsd(src: Operand, dst: Operand): Builder {
        return binary(Double_, BinaryOp.Mul, src, dst)
    }

    fun and(type: TypeX86, src: Operand, dst: Operand): Builder {
        return binary(type, BinaryOp.And, src, dst)
    }

    fun andq(src: Operand, dst: Operand): Builder {
        return and(Quadword, src, dst)
    }

    fun or(type: TypeX86, src: Operand, dst: Operand): Builder {
        return binary(type, BinaryOp.Or, src, dst)
    }

    fun orq(src: Operand, dst: Operand): Builder {
        return or(Quadword, src, dst)
    }

    fun xor(type: TypeX86, src: Operand, dst: Operand): Builder {
        return binary(type, BinaryOp.Xor, src, dst)
    }

    fun sal(type: TypeX86, src: Operand, dst: Operand): Builder {
        return binary(type, BinaryOp.ArithmeticLeftShift, src, dst)
    }

    fun sar(type: TypeX86, src: Operand, dst: Operand): Builder {
        return binary(type, BinaryOp.ArithmeticRightShift, src, dst)
    }

    fun shr(type: TypeX86, src: Operand, dst: Operand): Builder {
        return binary(type, BinaryOp.LogicalRightShift, src, dst)
    }

    fun shrq(src: Operand, dst: Operand): Builder {
        return shr(Quadword, src, dst)
    }

    fun cmp(type: TypeX86, src1: Operand, src2: Operand): Builder {
        instructions += Cmp(type, src1, src2)
        return this
    }

    fun comisd(src1: Operand, src2: Operand): Builder {
        return cmp(Double_, src1, src2)
    }

    fun cmp(type: TypeX86, i: Int, src2: Operand): Builder {
        return cmp(type, Imm(type, i), src2)
    }

    fun cmpq(src1: Operand, src2: Operand): Builder {
        return cmp(Quadword, src1, src2)
    }

    fun setcc(code: ConditionCode, src: Operand): Builder {
        instructions += SetCC(code, src)
        return this
    }

    fun sete(src: Operand): Builder {
        return setcc(E, src)
    }

    fun setne(src: Operand): Builder {
        return setcc(NE, src)
    }

    fun setg(src: Operand): Builder {
        return setcc(G, src)
    }

    fun setge(src: Operand): Builder {
        return setcc(GE, src)
    }

    fun setl(src: Operand): Builder {
        return setcc(L, src)
    }

    fun setle(src: Operand): Builder {
        return setcc(LE, src)
    }

    fun seta(src: Operand): Builder {
        return setcc(A, src)
    }

    fun setae(src: Operand): Builder {
        return setcc(AE, src)
    }

    fun setb(src: Operand): Builder {
        return setcc(B, src)
    }

    fun setbe(src: Operand): Builder {
        return setcc(BE, src)
    }

    fun jcc(code: ConditionCode, target: String): Builder {
        instructions += JmpCC(code, target)
        return this
    }

    fun je(target: String): Builder {
        return jcc(E, target)
    }

    fun jae(target: String): Builder {
        return jcc(AE, target)
    }

    fun jl(target: String): Builder {
        return jcc(L, target)
    }

    fun jne(target: String): Builder {
        return jcc(NE, target)
    }

    fun jp(target: String): Builder {
        return jcc(P, target)
    }

    fun jmp(target: String): Builder {
        instructions += Jmp(target)
        return this
    }

    fun label(identifier: String): Builder {
        instructions += Label(identifier)
        return this
    }

    fun push(value: Operand): Builder {
        instructions += Push(value)
        return this
    }

    fun call(name: String): Builder {
        instructions += Call(name)
        return this
    }

    fun cvttsd2si(dstType: TypeX86, src: Operand, dst: Operand): Builder {
        instructions += Cvttsd2si(dstType, src, dst)
        return this
    }

    fun cvttsd2siq(src: Operand, dst: Operand): Builder {
        return cvttsd2si(Quadword, src, dst)
    }

    fun cvtsi2sd(dstType: TypeX86, src: Operand, dst: Operand): Builder {
        instructions += Cvtsi2sd(dstType, src, dst)
        return this
    }

    fun cvtsi2sdq(src: Operand, dst: Operand): Builder {
        return cvtsi2sd(Quadword, src, dst)
    }

    fun cvtsi2sdl(src: Operand, dst: Operand): Builder {
        return cvtsi2sd(Longword, src, dst)
    }

    fun lea(src: Operand, dst: Operand): Builder {
        instructions += Lea(src, dst)
        return this
    }

    fun allocate(i: Int): Builder {
        return sub(Quadword, Imm(Quadword, i), SP)
    }

    fun deallocate(i: Int): Builder {
        return add(Quadword, Imm(Quadword, i), SP)
    }

    fun makelabel(name: String) = "${name}.${labelIndex++}"
}

private var labelIndex: Int = 0

fun buildX86(block: Builder.() -> Unit): List<Instruction> = with(Builder()) {
    block(this)
    return instructions
}