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
import eu.jameshamilton.tacky.UnaryOp.Complement
import eu.jameshamilton.tacky.UnaryOp.Negate
import eu.jameshamilton.tacky.UnaryOp.Not
import eu.jameshamilton.codegen.FunctionDef as x86FunctionDef
import eu.jameshamilton.codegen.Instruction as x86Instruction
import eu.jameshamilton.codegen.Program as x86Program
import eu.jameshamilton.tacky.Binary as TackyBinary
import eu.jameshamilton.tacky.Constant as TackyConstant
import eu.jameshamilton.tacky.FunctionDef as TackyFunctionDef
import eu.jameshamilton.tacky.Instruction as TackyInstruction
import eu.jameshamilton.tacky.Program as TackyProgram
import eu.jameshamilton.tacky.Unary as TackyUnary
import eu.jameshamilton.tacky.Value as TackyValue
import eu.jameshamilton.tacky.Var as TackyVar

fun convert(tackyProgram: TackyProgram): x86Program =
    // TODO: first..
    x86Program(convert(tackyProgram.functionDef.first()))

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
                val src = convert(tacky.value)
                mov(src, AX)
                ret()
            }

            is TackyUnary -> {
                val src = convert(tacky.src)
                val dst = convert(tacky.dst)
                mov(src, dst)
                when (tacky.op) {
                    Complement -> not(dst)
                    Negate -> neg(dst)
                    Not -> {
                        cmp(0, src)
                        // zero out register, as set instructions take 1 byte operands
                        mov(0, dst)
                        sete(dst)
                    }
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

                    LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual, Equal, NotEqual -> {
                        cmp(src2, src1)
                        mov(0, dst)
                        when (tacky.op) {
                            LessThan -> setl(dst)
                            LessThanOrEqual -> setle(dst)
                            GreaterThan -> setg(dst)
                            GreaterThanOrEqual -> setge(dst)
                            Equal -> sete(dst)
                            NotEqual -> setne(dst)
                            else -> throw RuntimeException("Invalid comparison operator ${tacky.op}.")
                        }
                    }

                    Add -> mov(src1, dst).add(src2, dst)
                    Subtract -> mov(src1, dst).sub(src2, dst)
                    Multiply -> mov(src1, dst).imul(src2, dst)
                    And -> mov(src1, dst).and(src2, dst)
                    Or -> mov(src1, dst).or(src2, dst)
                    Xor -> mov(src1, dst).xor(src2, dst)
                    LeftShift -> mov(src1, dst).sal(src2, dst)
                    RightShift -> mov(src1, dst).sar(src2, dst)
                }
            }

            is Copy -> {
                val src = convert(tacky.src)
                val dst = convert(tacky.dst)
                mov(src, dst)
            }

            is Jump -> {
                jmp(tacky.target)
            }

            is JumpIfNotZero -> {
                cmp(0, convert(tacky.condition))
                jne(tacky.target)
            }

            is JumpIfZero -> {
                cmp(0, convert(tacky.condition))
                je(tacky.target)
            }

            is Label -> label(tacky.identifier)
        }
    }
}
