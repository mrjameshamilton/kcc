package eu.jameshamilton.codegen

import eu.jameshamilton.codegen.RegisterAlias.EAX
import eu.jameshamilton.codegen.RegisterAlias.RAX
import eu.jameshamilton.codegen.RegisterName.AX
import eu.jameshamilton.codegen.RegisterName.CX
import eu.jameshamilton.codegen.RegisterName.DI
import eu.jameshamilton.codegen.RegisterName.DX
import eu.jameshamilton.codegen.RegisterName.R8
import eu.jameshamilton.codegen.RegisterName.R9
import eu.jameshamilton.codegen.RegisterName.SI
import eu.jameshamilton.frontend.FunType
import eu.jameshamilton.frontend.IntType
import eu.jameshamilton.frontend.LongType
import eu.jameshamilton.frontend.UIntType
import eu.jameshamilton.frontend.ULongType
import eu.jameshamilton.frontend.Unknown
import eu.jameshamilton.frontend.check.FunAttr
import eu.jameshamilton.frontend.check.LocalAttr
import eu.jameshamilton.frontend.check.StaticAttr
import eu.jameshamilton.frontend.check.symbolTable
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
import eu.jameshamilton.tacky.FunctionCall
import eu.jameshamilton.tacky.Jump
import eu.jameshamilton.tacky.JumpIfNotZero
import eu.jameshamilton.tacky.JumpIfZero
import eu.jameshamilton.tacky.Label
import eu.jameshamilton.tacky.Return
import eu.jameshamilton.tacky.SignExtend
import eu.jameshamilton.tacky.Truncate
import eu.jameshamilton.tacky.UnaryOp.Complement
import eu.jameshamilton.tacky.UnaryOp.Negate
import eu.jameshamilton.tacky.UnaryOp.Not
import eu.jameshamilton.unreachable
import eu.jameshamilton.codegen.FunctionDef as x86FunctionDef
import eu.jameshamilton.codegen.Instruction as x86Instruction
import eu.jameshamilton.codegen.Program as x86Program
import eu.jameshamilton.tacky.Binary as TackyBinary
import eu.jameshamilton.tacky.Constant as TackyConstant
import eu.jameshamilton.tacky.FunctionDef as TackyFunctionDef
import eu.jameshamilton.tacky.Instruction as TackyInstruction
import eu.jameshamilton.tacky.Program as TackyProgram
import eu.jameshamilton.tacky.StaticVariable as TackyStaticVariable
import eu.jameshamilton.tacky.Unary as TackyUnary
import eu.jameshamilton.tacky.Value as TackyValue
import eu.jameshamilton.tacky.Var as TackyVar

// TODO: only storing static here, types are in AST nodes, keep it like that?
private val backendSymbolTable: Map<String, Boolean> by lazy {
    symbolTable.map {
        val key = it.key
        val isStatic = when (it.value.attr) {
            is FunAttr, LocalAttr, null -> false
            is StaticAttr -> true
        }
        key to isStatic
    }.toMap()
}

val Pseudo.isStatic: Boolean
    get() = backendSymbolTable[identifier] == true

fun convert(tackyProgram: TackyProgram): x86Program =
    x86Program(tackyProgram.items.map {
        when (it) {
            is TackyStaticVariable -> convert(it)
            is TackyFunctionDef -> convert(it)
        }
    })

private fun convert(staticVariable: TackyStaticVariable): StaticVariable =
    StaticVariable(
        staticVariable.name,
        staticVariable.global,
        alignment = if (staticVariable.type == IntType) 4 else 8,
        staticVariable.init
    )

private fun convert(tackyFunctionDef: TackyFunctionDef): x86FunctionDef {
    val instructions = convert(tackyFunctionDef.instructions)

    val prologue = buildX86 {
        tackyFunctionDef.parameters.forEachIndexed { index, (tackyType, param) ->
            val type = when (tackyType) {
                is FunType, Unknown -> unreachable("Invalid type")
                IntType, UIntType -> Longword
                LongType, ULongType -> Quadword
            }
            // Copy all parameters from registers into stack locations,
            // to simplify things. Later, when register allocation is
            // implemented then memory/register use will be optimized.
            when (index) {
                // First six parameters are passed in registers.
                0 -> mov(type, DI.x(type), Pseudo(type, param))
                1 -> mov(type, SI.x(type), Pseudo(type, param))
                2 -> mov(type, DX.x(type), Pseudo(type, param))
                3 -> mov(type, CX.x(type), Pseudo(type, param))
                4 -> mov(type, R8.x(type), Pseudo(type, param))
                5 -> mov(type, R9.x(type), Pseudo(type, param))
                else -> {
                    // The rest on the stack.

                    // base address of callers stack frame + return address of caller,
                    // are on the stack before the parameters.
                    val stackParameterOffset = 2
                    // Then push the parameters in reverse order.
                    val parameterIndex = index - 6

                    val alignment = 8

                    mov(
                        type,
                        Stack(position = ((stackParameterOffset + parameterIndex) * alignment)),
                        Pseudo(type, param)
                    )
                }
            }
        }
    }

    return x86FunctionDef(
        tackyFunctionDef.name,
        tackyFunctionDef.global,
        tackyFunctionDef.defined,
        prologue + instructions
    )
}

private fun convert(instructions: List<TackyInstruction>): List<x86Instruction> = instructions.flatMap { tacky ->

    fun convert(value: TackyValue): Operand = when (value) {
        is TackyConstant -> when (value.value) {
            is Int -> Imm(Longword, value.value)
            else -> Imm(Quadword, value.value)
        }

        is TackyVar -> when (value.type) {
            IntType, UIntType -> Pseudo(Longword, value.name)
            LongType, ULongType -> Pseudo(Quadword, value.name)
            is FunType, Unknown -> unreachable("invalid type")
        }
    }

    buildX86 {
        when (tacky) {
            is Return -> {
                val src = convert(tacky.value)
                mov(src.type, src, AX.x(src.type))
                ret()
            }

            is TackyUnary -> {
                val src = convert(tacky.src)
                val dst = convert(tacky.dst)
                mov(src.type, src, dst)
                when (tacky.op) {
                    Complement -> not(src.type, dst)
                    Negate -> neg(src.type, dst)
                    Not -> {
                        cmp(src.type, 0, src)
                        // zero out register, as set instructions take 1 byte operands
                        mov(src.type, 0, dst)
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
                        mov(src1.type, src1, AX.x(src1.type))
                        cdq(src1.type)
                        idiv(src1.type, src2)
                        mov(src1.type, if (tacky.op == Divide) AX.x(src1.type) else DX.x(src1.type), dst)
                    }

                    LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual, Equal, NotEqual -> {
                        cmp(src1.type, src2, src1)
                        mov(dst.type, 0, dst)
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

                    Add -> mov(src1.type, src1, dst).add(src1.type, src2, dst)
                    Subtract -> mov(src1.type, src1, dst).sub(src1.type, src2, dst)
                    Multiply -> mov(src1.type, src1, dst).imul(src1.type, src2, dst)
                    And -> mov(src1.type, src1, dst).and(src1.type, src2, dst)
                    Or -> mov(src1.type, src1, dst).or(src1.type, src2, dst)
                    Xor -> mov(src1.type, src1, dst).xor(src1.type, src2, dst)
                    LeftShift -> mov(src1.type, src1, dst).sal(src1.type, src2, dst)
                    RightShift -> mov(src1.type, src1, dst).sar(src1.type, src2, dst)
                }
            }

            is Copy -> {
                val src = convert(tacky.src)
                val dst = convert(tacky.dst)
                mov(src.type, src, dst)
            }

            is Jump -> {
                jmp(tacky.target)
            }

            is JumpIfNotZero -> {
                val condition = convert(tacky.condition)
                cmp(condition.type, 0, condition)
                jne(tacky.target)
            }

            is JumpIfZero -> {
                val condition = convert(tacky.condition)
                cmp(condition.type, 0, condition)
                je(tacky.target)
            }

            is Label -> label(tacky.identifier)
            is FunctionCall -> {
                val registerArguments = tacky.arguments.take(6)
                val stackArguments = tacky.arguments.drop(6)
                val stackPadding = if (stackArguments.size % 2 == 0) 0 else 8

                if (stackPadding > 0) {
                    allocate(stackPadding)
                }

                registerArguments.forEachIndexed { index, param ->
                    // Copy all parameters from registers into stack locations,
                    // to simplify things. Later, when register allocation is
                    // implemented then memory/register use will be optimized.
                    val src = convert(param)
                    when (index) {
                        0 -> mov(src.type, src, DI.x(src.type))
                        1 -> mov(src.type, src, SI.x(src.type))
                        2 -> mov(src.type, src, DX.x(src.type))
                        3 -> mov(src.type, src, CX.x(src.type))
                        4 -> mov(src.type, src, R8.x(src.type))
                        5 -> mov(src.type, src, R9.x(src.type))
                    }
                }

                stackArguments.asReversed().forEach { param ->
                    val argument = convert(param)
                    if (argument is Register || argument is Imm || argument.type is Quadword) {
                        push(argument)
                    } else {
                        movl(argument, EAX)
                        push(RAX)
                    }
                }

                call(tacky.name)

                val bytesToRemove = (8 * stackArguments.size) + stackPadding
                if (bytesToRemove > 0) {
                    deallocate(bytesToRemove)
                }

                val result = convert(tacky.dst)
                mov(result.type, AX.x(result.type), result)
            }

            is SignExtend -> {
                val src = convert(tacky.src)
                val dst = convert(tacky.dst)
                movsx(src, dst)
            }

            is Truncate -> {
                val src = convert(tacky.src)
                val dst = convert(tacky.dst)
                // GNU assembler will issue a warning if the value is 8-bytes here,
                // so truncate it ourselves if needed.
                val truncated = when {
                    src is Imm && src.value is Long -> Imm(Longword, src.value.toInt())
                    else -> src
                }
                movl(truncated, dst)
            }
        }
    }
}
