package eu.jameshamilton.codegen

import eu.jameshamilton.codegen.ConditionCode.E
import eu.jameshamilton.codegen.ConditionCode.G
import eu.jameshamilton.codegen.ConditionCode.GE
import eu.jameshamilton.codegen.ConditionCode.L
import eu.jameshamilton.codegen.ConditionCode.LE
import eu.jameshamilton.codegen.ConditionCode.NE

class Builder(var instructions: List<Instruction> = mutableListOf()) {
    fun mov(src: Operand, dst: Operand): Builder {
        instructions += Mov(src, dst)
        return this
    }

    fun mov(i: Int, dst: Operand): Builder {
        return mov(Imm(i), dst)
    }

    fun mov(registerName: RegisterName, dst: Operand): Builder {
        return mov(Register(registerName), dst)
    }

    fun mov(src: Operand, dst: RegisterName): Builder {
        return mov(src, Register(dst))
    }

    fun cdq(): Builder {
        instructions += Cdq
        return this
    }

    fun idiv(registerName: RegisterName): Builder {
        return idiv(Register(registerName))
    }

    fun idiv(operand: Operand): Builder {
        instructions += IDiv(operand)
        return this
    }

    fun ret(): Builder {
        instructions += Ret
        return this
    }

    fun binary(binaryOp: BinaryOp, src: Operand, dst: Operand): Builder {
        instructions += Binary(binaryOp, src, dst)
        return this
    }

    fun binary(binaryOp: BinaryOp, src: RegisterName, dst: Operand): Builder {
        instructions += Binary(binaryOp, Register(src), dst)
        return this
    }

    fun unary(unaryOp: UnaryOp, src: Operand): Builder {
        instructions += Unary(unaryOp, src)
        return this
    }

    fun not(src: Operand): Builder {
        return unary(UnaryOp.Not, src)
    }

    fun neg(src: Operand): Builder {
        return unary(UnaryOp.Neg, src)
    }

    fun add(src: Operand, dst: Operand): Builder {
        return binary(BinaryOp.Add, src, dst)
    }

    fun sub(src: Operand, dst: Operand): Builder {
        return binary(BinaryOp.Sub, src, dst)
    }

    fun imul(src: Operand, dst: Operand): Builder {
        return binary(BinaryOp.Mul, src, dst)
    }

    fun imul(src: Operand, dst: RegisterName): Builder {
        return binary(BinaryOp.Mul, src, Register(dst))
    }

    fun and(src: Operand, dst: Operand): Builder {
        return binary(BinaryOp.And, src, dst)
    }

    fun or(src: Operand, dst: Operand): Builder {
        return binary(BinaryOp.Or, src, dst)
    }

    fun xor(src: Operand, dst: Operand): Builder {
        return binary(BinaryOp.Xor, src, dst)
    }

    fun sal(src: Operand, dst: Operand): Builder {
        return binary(BinaryOp.LeftShift, src, dst)
    }

    fun sar(src: Operand, dst: Operand): Builder {
        return binary(BinaryOp.RightShift, src, dst)
    }

    fun cmp(src1: Operand, src2: Operand): Builder {
        instructions += Cmp(src1, src2)
        return this
    }

    fun cmp(i: Int, src2: Operand): Builder {
        return cmp(Imm(i), src2)
    }

    fun cmp(registerName: RegisterName, src2: Operand): Builder {
        return cmp(Register(registerName), src2)
    }

    fun cmp(src1: Operand, registerName: RegisterName): Builder {
        return cmp(src1, Register(registerName))
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
}

fun buildX86(block: Builder.() -> Unit): List<Instruction> = with(Builder()) {
    block(this)
    return instructions
}