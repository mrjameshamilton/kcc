package eu.jameshamilton.codegen

import eu.jameshamilton.codegen.RegisterAlias.BP
import eu.jameshamilton.codegen.RegisterAlias.EAX
import eu.jameshamilton.codegen.RegisterAlias.RAX
import eu.jameshamilton.codegen.RegisterAlias.RDX
import eu.jameshamilton.codegen.RegisterAlias.XMM0
import eu.jameshamilton.codegen.RegisterAlias.XMM1
import eu.jameshamilton.codegen.RegisterAlias.XMM2
import eu.jameshamilton.codegen.RegisterAlias.XMM3
import eu.jameshamilton.codegen.RegisterAlias.XMM4
import eu.jameshamilton.codegen.RegisterAlias.XMM5
import eu.jameshamilton.codegen.RegisterAlias.XMM6
import eu.jameshamilton.codegen.RegisterAlias.XMM7
import eu.jameshamilton.codegen.RegisterName.AX
import eu.jameshamilton.codegen.RegisterName.CX
import eu.jameshamilton.codegen.RegisterName.DI
import eu.jameshamilton.codegen.RegisterName.DX
import eu.jameshamilton.codegen.RegisterName.R8
import eu.jameshamilton.codegen.RegisterName.R9
import eu.jameshamilton.codegen.RegisterName.SI
import eu.jameshamilton.frontend.ArrayType
import eu.jameshamilton.frontend.DoubleType
import eu.jameshamilton.frontend.FunType
import eu.jameshamilton.frontend.IntType
import eu.jameshamilton.frontend.IntegerType
import eu.jameshamilton.frontend.LongType
import eu.jameshamilton.frontend.PointerType
import eu.jameshamilton.frontend.UIntType
import eu.jameshamilton.frontend.ULongType
import eu.jameshamilton.frontend.Unknown
import eu.jameshamilton.frontend.check.FunAttr
import eu.jameshamilton.frontend.check.LocalAttr
import eu.jameshamilton.frontend.check.StaticAttr
import eu.jameshamilton.frontend.check.symbolTable
import eu.jameshamilton.frontend.isSigned
import eu.jameshamilton.tacky.BinaryOp.Add
import eu.jameshamilton.tacky.BinaryOp.And
import eu.jameshamilton.tacky.BinaryOp.Divide
import eu.jameshamilton.tacky.BinaryOp.Equal
import eu.jameshamilton.tacky.BinaryOp.GreaterThan
import eu.jameshamilton.tacky.BinaryOp.GreaterThanOrEqual
import eu.jameshamilton.tacky.BinaryOp.LeftShift
import eu.jameshamilton.tacky.BinaryOp.LessThan
import eu.jameshamilton.tacky.BinaryOp.LessThanOrEqual
import eu.jameshamilton.tacky.BinaryOp.LogicalRightShift
import eu.jameshamilton.tacky.BinaryOp.Multiply
import eu.jameshamilton.tacky.BinaryOp.NotEqual
import eu.jameshamilton.tacky.BinaryOp.Or
import eu.jameshamilton.tacky.BinaryOp.Remainder
import eu.jameshamilton.tacky.BinaryOp.RightShift
import eu.jameshamilton.tacky.BinaryOp.Subtract
import eu.jameshamilton.tacky.BinaryOp.Xor
import eu.jameshamilton.tacky.Constant
import eu.jameshamilton.tacky.Copy
import eu.jameshamilton.tacky.DoubleToInt
import eu.jameshamilton.tacky.DoubleToUInt
import eu.jameshamilton.tacky.FunctionCall
import eu.jameshamilton.tacky.GetAddress
import eu.jameshamilton.tacky.IntToDouble
import eu.jameshamilton.tacky.Jump
import eu.jameshamilton.tacky.JumpIfNotZero
import eu.jameshamilton.tacky.JumpIfZero
import eu.jameshamilton.tacky.Label
import eu.jameshamilton.tacky.Load
import eu.jameshamilton.tacky.Return
import eu.jameshamilton.tacky.SignExtend
import eu.jameshamilton.tacky.Store
import eu.jameshamilton.tacky.Truncate
import eu.jameshamilton.tacky.UIntToDouble
import eu.jameshamilton.tacky.UnaryOp.Complement
import eu.jameshamilton.tacky.UnaryOp.Negate
import eu.jameshamilton.tacky.UnaryOp.Not
import eu.jameshamilton.tacky.Var
import eu.jameshamilton.tacky.ZeroExtend
import eu.jameshamilton.unreachable
import eu.jameshamilton.codegen.Double_ as x86DoubleType
import eu.jameshamilton.codegen.FunctionDef as x86FunctionDef
import eu.jameshamilton.codegen.Instruction as x86Instruction
import eu.jameshamilton.codegen.Program as x86Program
import eu.jameshamilton.codegen.Unknown as x86UnknownType
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

val TackyValue.isSigned: Boolean
    get() = when (this) {
        is Constant -> when (value) {
            is Int, is Long -> true
            else -> false
        }

        is Var -> type is IntegerType && type.isSigned
    }

val Data.isConstant: Boolean
    get() = constants.containsValue(identifier)

private data class UnnamedStaticConstant(val alignment: Int, val value: Any)

private val constants: MutableMap<UnnamedStaticConstant, String> = mutableMapOf()

private fun makeconstant(value: Any, alignment: Int = 8): Data =
    Data(x86DoubleType, constants.computeIfAbsent(UnnamedStaticConstant(alignment, value)) { _ ->
        "C" + constants.size
    })

private fun zero(type: TypeX86) = if (type is x86DoubleType) makeconstant(0.0) else Imm(type, 0)

fun convert(tackyProgram: TackyProgram): x86Program {
    val topLevelItems = tackyProgram.items.map {
        when (it) {
            is TackyStaticVariable -> convert(it)
            is TackyFunctionDef -> convert(it)
        }
    }

    val constants = constants.map { (constant, name) ->
        StaticConstant(name, constant.alignment, constant.value)
    }

    return x86Program(topLevelItems + constants)
}

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
        val parameters = tackyFunctionDef.parameters.mapIndexed { index, (tackyType, param) ->
            val x86Type = when (tackyType) {
                is FunType, Unknown -> unreachable("Invalid type")
                IntType, UIntType -> Longword
                LongType, ULongType, is PointerType -> Quadword
                DoubleType -> x86DoubleType
                is ArrayType -> TODO()
            }
            Triple(index, x86Type, param)
        }

        val integerParameters = parameters.filter { it.second !is x86DoubleType }.take(6)
        val doubleParameters = parameters.filter { it.second is x86DoubleType }.take(8)
        val stackParameters = parameters - (integerParameters + doubleParameters).toSet()

        // Copy all parameters from registers into stack locations,
        // to simplify things. Later, when register allocation is
        // implemented then memory/register use will be optimized.
        integerParameters.forEachIndexed { index, (_, type, param) ->
            when (index) {
                // First six parameters are passed in registers.
                0 -> mov(type, DI.x(type), Pseudo(type, param))
                1 -> mov(type, SI.x(type), Pseudo(type, param))
                2 -> mov(type, DX.x(type), Pseudo(type, param))
                3 -> mov(type, CX.x(type), Pseudo(type, param))
                4 -> mov(type, R8.x(type), Pseudo(type, param))
                5 -> mov(type, R9.x(type), Pseudo(type, param))
            }
        }

        doubleParameters.forEachIndexed { index, (_, _, param) ->
            when (index) {
                // First 8 doubles are passed in registers.
                0 -> movsd(XMM0, Pseudo(x86DoubleType, param))
                1 -> movsd(XMM1, Pseudo(x86DoubleType, param))
                2 -> movsd(XMM2, Pseudo(x86DoubleType, param))
                3 -> movsd(XMM3, Pseudo(x86DoubleType, param))
                4 -> movsd(XMM4, Pseudo(x86DoubleType, param))
                5 -> movsd(XMM5, Pseudo(x86DoubleType, param))
                6 -> movsd(XMM6, Pseudo(x86DoubleType, param))
                7 -> movsd(XMM7, Pseudo(x86DoubleType, param))
            }
        }

        // Then push the remaining parameters in reverse order.
        // The base address of callers stack frame + return address of caller,
        // are on the stack before the parameters.
        val stackParameterOffset = 2
        val size = 8
        stackParameters
            .mapIndexed { index, (_, type, param) -> Triple(index + stackParameterOffset, type, param) }
            .asReversed()
            .forEach { (index, type, param) ->
                mov(type, Mem(BP, position = index * size), Pseudo(type, param))
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
            is Int, is UInt -> Imm(Longword, value.value)
            is Long, is ULong -> Imm(Quadword, value.value)
            is Double -> makeconstant(value.value)
            else -> unreachable("Invalid type ")
        }

        is TackyVar -> when (value.type) {
            IntType, UIntType -> Pseudo(Longword, value.name)
            LongType, ULongType -> Pseudo(Quadword, value.name)
            is PointerType -> Pseudo(Quadword, value.name)
            DoubleType -> Pseudo(x86DoubleType, value.name)
            is FunType, Unknown -> unreachable("Invalid type")
            is ArrayType -> TODO()
        }
    }


    buildX86 {
        when (tacky) {
            is Return -> {
                val src = convert(tacky.value)
                if (src.type is x86DoubleType) {
                    movsd(src, XMM0)
                } else {
                    mov(src.type, src, AX.x(src.type))
                }
                ret()
            }

            is TackyUnary -> {
                val src = convert(tacky.src)
                val dst = convert(tacky.dst)

                mov(src.type, src, dst)

                when (tacky.op) {
                    Complement -> not(src.type, dst)
                    Negate -> {
                        if (src.type is x86DoubleType) {
                            movsd(src, XMM0)
                            xor(x86DoubleType, makeconstant(-0.0, 16), XMM0)
                            movsd(XMM0, dst)
                        } else {
                            neg(src.type, dst)
                        }
                    }

                    Not -> {
                        if (src.type is x86DoubleType) {
                            // zero out the full XMM0 register before comparison
                            xor(x86DoubleType, XMM0, XMM0)
                            comisd(src, XMM0)
                        } else {
                            cmp(src.type, zero(src.type), src)
                        }
                        // zero out register, as set instructions take 1 byte operands
                        mov(src.type, zero(src.type), dst)
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
                        if (src1.type is x86DoubleType) {
                            // Only division since % is not applicable for double.
                            movsd(src1, XMM0)
                            movsd(src2, XMM1)
                            divdouble(x86DoubleType, XMM1, XMM0)
                            movsd(XMM0, dst)
                        } else {
                            // division / remainder use EAX + EDX together:
                            // the division result is stored in EAX; the remainder in EDX.
                            mov(src1.type, src1, AX.x(src1.type))
                            if (tacky.src1.isSigned) {
                                cdq(src1.type)
                                idiv(src1.type, src2)
                            } else {
                                // zero extend the result by zeroing out RDX
                                movq(zero(Quadword), RDX)
                                div(src1.type, src2)
                            }
                            mov(src1.type, if (tacky.op == Divide) AX.x(src1.type) else DX.x(src1.type), dst)
                        }
                    }

                    LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual, Equal, NotEqual -> {
                        cmp(src1.type, src2, src1)
                        val isNaN = makelabel("isnan")
                        val end = makelabel("notnan")
                        if (src1.type is x86DoubleType) {
                            jp(isNaN)
                        }
                        // zero out the dst before setting the byte with set*.
                        mov(dst.type, zero(dst.type), dst)

                        if (tacky.src1.isSigned) {
                            when (tacky.op) {
                                LessThan -> setl(dst)
                                LessThanOrEqual -> setle(dst)
                                GreaterThan -> setg(dst)
                                GreaterThanOrEqual -> setge(dst)
                                Equal -> sete(dst)
                                NotEqual -> setne(dst)
                                else -> throw RuntimeException("Invalid comparison operator ${tacky.op}.")
                            }
                        } else {
                            when (tacky.op) {
                                LessThan -> setb(dst)
                                LessThanOrEqual -> setbe(dst)
                                GreaterThan -> seta(dst)
                                GreaterThanOrEqual -> setae(dst)
                                Equal -> sete(dst)
                                NotEqual -> setne(dst)
                                else -> throw RuntimeException("Invalid comparison operator ${tacky.op}.")
                            }
                        }
                        if (src1.type is x86DoubleType) {
                            jmp(end)
                            label(isNaN)
                            when (tacky.op) {
                                LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual, Equal -> {
                                    mov(dst.type, Imm(dst.type, 0), dst)
                                }

                                NotEqual -> {
                                    mov(dst.type, Imm(dst.type, 1), dst)
                                }

                                else -> throw RuntimeException("Invalid comparison operator ${tacky.op}.")
                            }
                            label(end)
                        }
                    }

                    Add -> mov(src1.type, src1, dst).add(src1.type, src2, dst)
                    Subtract -> mov(src1.type, src1, dst).sub(src1.type, src2, dst)
                    Multiply -> mov(src1.type, src1, dst).mul(src1.type, src2, dst)
                    And -> mov(src1.type, src1, dst).and(src1.type, src2, dst)
                    Or -> mov(src1.type, src1, dst).or(src1.type, src2, dst)
                    Xor -> mov(src1.type, src1, dst).xor(src1.type, src2, dst)
                    LeftShift -> mov(src1.type, src1, dst).sal(src1.type, src2, dst)
                    RightShift -> mov(src1.type, src1, dst).sar(src1.type, src2, dst)
                    LogicalRightShift -> mov(src1.type, src1, dst).shr(src1.type, src2, dst)
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
                if (condition.type is x86DoubleType) {
                    // zero out the full XMM0 register before comparison
                    xor(x86DoubleType, XMM0, XMM0)
                    comisd(XMM0, condition)
                } else {
                    cmp(condition.type, zero(condition.type), condition)
                }
                jne(tacky.target)
            }

            is JumpIfZero -> {
                val condition = convert(tacky.condition)
                if (condition.type is x86DoubleType) {
                    // zero out the full XMM0 register before comparison
                    xor(x86DoubleType, XMM0, XMM0)
                    comisd(XMM0, condition)
                } else {
                    cmp(condition.type, zero(condition.type), condition)
                }
                je(tacky.target)
            }

            is Label -> label(tacky.identifier)
            is FunctionCall -> {
                val arguments = tacky.arguments.mapIndexed { index, arg -> Pair(index, convert(arg)) }
                val integerArguments = arguments.filter { it.second.type !is x86DoubleType }.take(6)
                val doubleArguments = arguments.filter { it.second.type is x86DoubleType }.take(8)
                val stackArguments = arguments - (integerArguments + doubleArguments).toSet()

                val stackPadding = if (stackArguments.size % 2 == 0) 0 else 8

                if (stackPadding > 0) {
                    allocate(stackPadding)
                }

                integerArguments.forEachIndexed { index, (originalIndex, arg) ->
                    when (index) {
                        0 -> mov(arg.type, arg, DI.x(arg.type))
                        1 -> mov(arg.type, arg, SI.x(arg.type))
                        2 -> mov(arg.type, arg, DX.x(arg.type))
                        3 -> mov(arg.type, arg, CX.x(arg.type))
                        4 -> mov(arg.type, arg, R8.x(arg.type))
                        5 -> mov(arg.type, arg, R9.x(arg.type))
                    }
                }

                doubleArguments.forEachIndexed { index, (_, arg) ->
                    when (index) {
                        0 -> mov(arg.type, arg, XMM0)
                        1 -> mov(arg.type, arg, XMM1)
                        2 -> mov(arg.type, arg, XMM2)
                        3 -> mov(arg.type, arg, XMM3)
                        4 -> mov(arg.type, arg, XMM4)
                        5 -> mov(arg.type, arg, XMM5)
                        6 -> mov(arg.type, arg, XMM6)
                        7 -> mov(arg.type, arg, XMM7)
                    }
                }

                stackArguments.asReversed().forEach { (_, arg) ->
                    if (arg is Register || arg is Imm || arg.type is Quadword || arg.type is x86DoubleType) {
                        push(arg)
                    } else {
                        movl(arg, EAX)
                        push(RAX)
                    }
                }

                call(tacky.name)

                val bytesToRemove = (8 * stackArguments.size) + stackPadding
                if (bytesToRemove > 0) {
                    deallocate(bytesToRemove)
                }

                val result = convert(tacky.dst)

                if (result.type is x86DoubleType) {
                    movsd(XMM0, result)
                } else {
                    mov(result.type, AX.x(result.type), result)
                }
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
                    src is Imm && src.value is ULong -> Imm(Longword, src.value.toUInt())
                    else -> src
                }
                movl(truncated, dst)
            }

            is ZeroExtend -> {
                val src = convert(tacky.src)
                val dst = convert(tacky.dst)
                movzx(src, dst)
            }

            is DoubleToInt -> {
                val src = convert(tacky.src)
                val dst = convert(tacky.dst)
                cvttsd2si(dst.type, src, dst)
            }

            is DoubleToUInt -> {
                val src = convert(tacky.src)
                val dst = convert(tacky.dst)

                when (dst.type) {
                    is Longword -> {
                        cvttsd2siq(src, AX.q)
                        movl(AX.d, dst)
                    }

                    is Quadword -> {
                        val MAX_LONG_PLUS_ONE = 9223372036854775808u // 2^63

                        val upperBound = makeconstant(MAX_LONG_PLUS_ONE)
                        val outOfRange = makelabel("dotui_out_of_range")
                        val end = makelabel("dotui_end")

                        val tmp1 = Pseudo(Quadword, makelabel("tmp1"))
                        val tmp2 = Pseudo(Quadword, makelabel("tmp2"))

                        comisd(upperBound, src)
                        jae(outOfRange)
                        cvttsd2siq(src, dst)
                        jmp(end)
                        label(outOfRange)

                        movsd(src, tmp1)
                        subsd(upperBound, tmp1)
                        cvttsd2siq(tmp1, dst)
                        movq(Imm(Quadword, MAX_LONG_PLUS_ONE), tmp2)
                        addq(tmp1, dst)
                        label(end)
                    }

                    x86DoubleType, x86UnknownType -> unreachable("invalid types")
                }
            }

            is IntToDouble -> {
                val src = convert(tacky.src)
                val dst = convert(tacky.dst)
                cvtsi2sd(src.type, src, dst)
            }

            is UIntToDouble -> {
                val src = convert(tacky.src)
                val dst = convert(tacky.dst)

                when (src.type) {
                    is Longword -> {
                        // zero extend, since moving it into a registers lower 32-bits
                        // will zero out the top 32 bits.
                        movl(src, AX.d)
                        cvtsi2sdq(AX.q, dst)
                    }

                    is Quadword -> {
                        // for unsigned long to double conversion, we must
                        // make sure that when halving the value we don't round
                        // to a midpoint between two values that double can represent.
                        // see page 320.

                        val outOfRange = makelabel("uitod_out_of_range")
                        val end = makelabel("uitod_end")

                        cmpq(zero(Quadword), src)
                        jl(outOfRange)
                        cvtsi2sdq(src, dst)
                        jmp(end)
                        label(outOfRange)

                        val tmp = Pseudo(Quadword, makelabel("tmp"))
                        val rounded = Pseudo(Quadword, makelabel("rounded"))

                        // round to odd
                        movq(src, tmp)
                        movq(tmp, rounded)
                        shrq(Imm(Quadword, 1), rounded)
                        andq(Imm(Quadword, 1), tmp)
                        orq(tmp, rounded)
                        // end round

                        cvtsi2sdq(rounded, dst)
                        addsd(dst, dst)
                        label(end)
                    }

                    is x86DoubleType, is x86UnknownType -> unreachable("Invalid type for src")
                }
            }

            is GetAddress -> {
                val src = convert(tacky.src)
                val dst = convert(tacky.dst)
                lea(src, dst)
            }

            is Load -> {
                val ptr = convert(tacky.ptr)
                val dst = convert(tacky.dst)

                movq(ptr, AX.q)
                mov(dst.type, Mem(AX.q, 0), dst)
            }

            is Store -> {
                val src = convert(tacky.src)
                val ptr = convert(tacky.ptr)

                movq(ptr, AX.q)
                mov(src.type, src, Mem(AX.q, 0))
            }
        }
    }
}
