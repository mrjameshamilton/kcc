package eu.jameshamilton.codegen

import eu.jameshamilton.codegen.RegisterName.AX
import eu.jameshamilton.codegen.RegisterName.DX
import eu.jameshamilton.tacky.Binary
import eu.jameshamilton.tacky.BinaryOp.Add
import eu.jameshamilton.tacky.BinaryOp.Divide
import eu.jameshamilton.tacky.BinaryOp.Multiply
import eu.jameshamilton.tacky.BinaryOp.Remainder
import eu.jameshamilton.tacky.BinaryOp.Subtract
import eu.jameshamilton.tacky.TackyReturn
import eu.jameshamilton.codegen.BinaryOp as x86BinaryOp
import eu.jameshamilton.codegen.FunctionDef as x86FunctionDef
import eu.jameshamilton.codegen.Instruction as x86Instruction
import eu.jameshamilton.codegen.Program as x86Program
import eu.jameshamilton.codegen.Unary as x86Unary
import eu.jameshamilton.codegen.UnaryOp as x86UnaryOp
import eu.jameshamilton.tacky.BinaryOp as TackyBinaryOp
import eu.jameshamilton.tacky.Constant as TackyConstant
import eu.jameshamilton.tacky.FunctionDef as TackyFunctionDef
import eu.jameshamilton.tacky.Instruction as TackyInstruction
import eu.jameshamilton.tacky.Program as TackyProgram
import eu.jameshamilton.tacky.Unary as TackyUnary
import eu.jameshamilton.tacky.UnaryOp as TackyUnaryOp
import eu.jameshamilton.tacky.Value as TackyValue
import eu.jameshamilton.tacky.Var as TackyVar

fun convert(tackyProgram: TackyProgram): x86Program =
    x86Program(convert(tackyProgram.functionDef))

private fun convert(tackyFunctionDef: TackyFunctionDef): x86FunctionDef =
    x86FunctionDef(tackyFunctionDef.name, convert(tackyFunctionDef.instructions))

private fun convert(instructions: List<TackyInstruction>): List<x86Instruction> = instructions.flatMap {

    fun convert(value: TackyValue): Operand = when (value) {
        is TackyConstant -> Imm(value.value)
        is TackyVar -> Pseudo(value.name)
    }

    fun convert(op: TackyUnaryOp): x86UnaryOp = when (op) {
        TackyUnaryOp.Complement -> x86UnaryOp.Not
        TackyUnaryOp.Negate -> x86UnaryOp.Neg
    }

    fun convert(op: TackyBinaryOp): x86BinaryOp = when (op) {
        Add -> x86BinaryOp.Add
        Subtract -> x86BinaryOp.Sub
        Multiply -> x86BinaryOp.Mul
        Divide -> TODO()
        Remainder -> TODO()
    }

    when (it) {
        is TackyReturn -> Mov(convert(it.value), Register(AX)) + Ret
        is TackyUnary -> Mov(convert(it.src), convert(it.dst)) + x86Unary(convert(it.op), convert(it.dst))
        is Binary -> when (it.op) {
            Divide, Remainder ->
                // division / remainder use EAX + EDX together:
                // the division result is stored in EAX; the remainder in EDX.
                Mov(convert(it.src1), Register(AX)) +
                        Cdq +
                        IDiv(convert(it.src2)) +
                        Mov(Register(if (it.op == Remainder) DX else AX), convert(it.dst))

            else -> Mov(convert(it.src1), convert(it.dst)) + Binary(convert(it.op), convert(it.src2), convert(it.dst))
        }
    }
}

