package eu.jameshamilton.codegen

import eu.jameshamilton.codegen.ConditionCode.E
import eu.jameshamilton.codegen.ConditionCode.G
import eu.jameshamilton.codegen.ConditionCode.GE
import eu.jameshamilton.codegen.ConditionCode.L
import eu.jameshamilton.codegen.ConditionCode.LE
import eu.jameshamilton.codegen.ConditionCode.NE
import eu.jameshamilton.codegen.RegisterAlias.SP

class Builder(var instructions: List<Instruction> = mutableListOf()) {
    fun mov(type: TypeX86, src: Operand, dst: Operand): Builder {
        instructions += Mov(type, src, dst)
        return this
    }

    fun movl(src: Operand, dst: Operand): Builder {
        return mov(Longword, src, dst)
    }

    fun movq(src: Operand, dst: Operand): Builder {
        return mov(Quadword, src, dst)
    }

    fun mov(type: TypeX86, i: Int, dst: Operand): Builder {
        return mov(type, Imm(type, i), dst)
    }

    fun movsx(src: Operand, dst: Operand): Builder {
        instructions += Movsx(src, dst)
        return this
    }

    fun cdq(type: TypeX86): Builder {
        instructions += Cdq(type)
        return this
    }

    fun idiv(type: TypeX86, operand: Operand): Builder {
        instructions += IDiv(type, operand)
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

    fun sub(type: TypeX86, src: Operand, dst: Operand): Builder {
        return binary(type, BinaryOp.Sub, src, dst)
    }

    fun imul(type: TypeX86, src: Operand, dst: Operand): Builder {
        return binary(type, BinaryOp.Mul, src, dst)
    }

    fun and(type: TypeX86, src: Operand, dst: Operand): Builder {
        return binary(type, BinaryOp.And, src, dst)
    }

    fun or(type: TypeX86, src: Operand, dst: Operand): Builder {
        return binary(type, BinaryOp.Or, src, dst)
    }

    fun xor(type: TypeX86, src: Operand, dst: Operand): Builder {
        return binary(type, BinaryOp.Xor, src, dst)
    }

    fun sal(type: TypeX86, src: Operand, dst: Operand): Builder {
        return binary(type, BinaryOp.LeftShift, src, dst)
    }

    fun sar(type: TypeX86, src: Operand, dst: Operand): Builder {
        return binary(type, BinaryOp.RightShift, src, dst)
    }

    fun cmp(type: TypeX86, src1: Operand, src2: Operand): Builder {
        instructions += Cmp(type, src1, src2)
        return this
    }

    fun cmp(type: TypeX86, i: Int, src2: Operand): Builder {
        return cmp(type, Imm(type, i), src2)
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

    fun jcc(code: ConditionCode, target: String): Builder {
        instructions += JmpCC(code, target)
        return this
    }

    fun je(target: String): Builder {
        return jcc(E, target)
    }

    fun jne(target: String): Builder {
        return jcc(NE, target)
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

    fun allocate(i: Int): Builder {
        return sub(Quadword, Imm(Quadword, i), SP)
    }

    fun deallocate(i: Int): Builder {
        return add(Quadword, Imm(Quadword, i), SP)
    }
}

fun buildX86(block: Builder.() -> Unit): List<Instruction> = with(Builder()) {
    block(this)
    return instructions
}