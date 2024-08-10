package eu.jameshamilton.codegen

import eu.jameshamilton.codegen.RegisterName.AX
import eu.jameshamilton.codegen.RegisterName.DX
import eu.jameshamilton.tacky.BinaryOp.Add
import eu.jameshamilton.tacky.BinaryOp.And
import eu.jameshamilton.tacky.BinaryOp.Divide
import eu.jameshamilton.tacky.BinaryOp.Equal
import eu.jameshamilton.tacky.BinaryOp.GreaterThan
import eu.jameshamilton.tacky.BinaryOp.GreaterThanOrEqual
import eu.jameshamilton.tacky.BinaryOp.LeftShift
import eu.jameshamilton.tacky.BinaryOp.LessThan
import eu.jameshamilton.tacky.BinaryOp.LessThanOrEqual
import eu.jameshamilton.tacky.BinaryOp.Multiply
import eu.jameshamilton.tacky.BinaryOp.NotEqual
import eu.jameshamilton.tacky.BinaryOp.Or
import eu.jameshamilton.tacky.BinaryOp.Remainder
import eu.jameshamilton.tacky.BinaryOp.RightShift
import eu.jameshamilton.tacky.BinaryOp.Subtract
import eu.jameshamilton.tacky.BinaryOp.Xor
import eu.jameshamilton.tacky.Copy
import eu.jameshamilton.tacky.Jump
import eu.jameshamilton.tacky.JumpIfNotZero
import eu.jameshamilton.tacky.JumpIfZero
import eu.jameshamilton.tacky.Label
import eu.jameshamilton.tacky.TackyReturn
import eu.jameshamilton.codegen.FunctionDef as x86FunctionDef
import eu.jameshamilton.codegen.Instruction as x86Instruction
import eu.jameshamilton.codegen.Program as x86Program
import eu.jameshamilton.tacky.Binary as TackyBinary
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

private fun convert(instructions: List<TackyInstruction>): List<x86Instruction> = instructions.flatMap { tacky ->

    fun convert(value: TackyValue): Operand = when (value) {
        is TackyConstant -> Imm(value.value)
        is TackyVar -> Pseudo(value.name)
    }

    buildX86 {
        when (tacky) {
            is TackyReturn -> {
                mov(convert(tacky.value), AX)
                ret()
            }

            is TackyUnary -> {
                val src = convert(tacky.src)
                val dst = convert(tacky.dst)
                mov(src, dst)
                when (tacky.op) {
                    TackyUnaryOp.Complement -> not(dst)
                    TackyUnaryOp.Negate -> neg(dst)
                    TackyUnaryOp.Not -> TODO()
                }
            }

            is TackyBinary -> {
                val src1 = convert(tacky.src1)
                val src2 = convert(tacky.src2)
                val dst = convert(tacky.dst)
                when (tacky.op) {
                    Divide, Remainder -> {
                        // division / remainder use EAX + EDX together:
                        // the division result is stored in EAX; the remainder in EDX.
                        mov(src1, AX)
                        cdq()
                        idiv(src2)
                        mov(if (tacky.op == Divide) AX else DX, dst)
                    }

                    else -> {
                        mov(src1, dst)
                        when (tacky.op) {
                            Add -> add(src2, dst)
                            Subtract -> sub(src2, dst)
                            Multiply -> imul(src2, dst)
                            And -> and(src2, dst)
                            Or -> or(src2, dst)
                            Xor -> xor(src2, dst)
                            LeftShift -> sal(src2, dst)
                            RightShift -> sar(src2, dst)
                            LessThan -> TODO()
                            LessThanOrEqual -> TODO()
                            GreaterThan -> TODO()
                            GreaterThanOrEqual -> TODO()
                            Equal -> TODO()
                            NotEqual -> TODO()
                            Divide, Remainder -> unreachable("special case")
                        }
                    }
                }
            }

            is Copy -> TODO()
            is Jump -> TODO()
            is JumpIfNotZero -> TODO()
            is JumpIfZero -> TODO()
            is Label -> TODO()
        }
    }
}

fun unreachable(reason: String): Nothing {
    throw RuntimeException("unreachable: $reason")
}
