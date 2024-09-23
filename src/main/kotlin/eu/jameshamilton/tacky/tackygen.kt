package eu.jameshamilton.tacky

import eu.jameshamilton.frontend.AddrOf
import eu.jameshamilton.frontend.ArrayType
import eu.jameshamilton.frontend.Assignment
import eu.jameshamilton.frontend.BinaryExpr
import eu.jameshamilton.frontend.BinaryOp
import eu.jameshamilton.frontend.BinaryOp.Add
import eu.jameshamilton.frontend.BinaryOp.And
import eu.jameshamilton.frontend.BinaryOp.Divide
import eu.jameshamilton.frontend.BinaryOp.Equal
import eu.jameshamilton.frontend.BinaryOp.GreaterThan
import eu.jameshamilton.frontend.BinaryOp.GreaterThanOrEqual
import eu.jameshamilton.frontend.BinaryOp.LeftShift
import eu.jameshamilton.frontend.BinaryOp.LessThan
import eu.jameshamilton.frontend.BinaryOp.LessThanOrEqual
import eu.jameshamilton.frontend.BinaryOp.LogicalAnd
import eu.jameshamilton.frontend.BinaryOp.LogicalOr
import eu.jameshamilton.frontend.BinaryOp.Multiply
import eu.jameshamilton.frontend.BinaryOp.NotEqual
import eu.jameshamilton.frontend.BinaryOp.Or
import eu.jameshamilton.frontend.BinaryOp.Remainder
import eu.jameshamilton.frontend.BinaryOp.RightShift
import eu.jameshamilton.frontend.BinaryOp.Subtract
import eu.jameshamilton.frontend.BinaryOp.Xor
import eu.jameshamilton.frontend.BlockItem
import eu.jameshamilton.frontend.Break
import eu.jameshamilton.frontend.Cast
import eu.jameshamilton.frontend.Compound
import eu.jameshamilton.frontend.CompoundInit
import eu.jameshamilton.frontend.Conditional
import eu.jameshamilton.frontend.Constant
import eu.jameshamilton.frontend.Continue
import eu.jameshamilton.frontend.DefaultCase
import eu.jameshamilton.frontend.Dereference
import eu.jameshamilton.frontend.DoWhile
import eu.jameshamilton.frontend.DoubleType
import eu.jameshamilton.frontend.Expression
import eu.jameshamilton.frontend.ExpressionCase
import eu.jameshamilton.frontend.ExpressionStatement
import eu.jameshamilton.frontend.For
import eu.jameshamilton.frontend.FunDeclaration
import eu.jameshamilton.frontend.FunctionCall
import eu.jameshamilton.frontend.Goto
import eu.jameshamilton.frontend.If
import eu.jameshamilton.frontend.InitDecl
import eu.jameshamilton.frontend.InitExpr
import eu.jameshamilton.frontend.Initializer
import eu.jameshamilton.frontend.IntType
import eu.jameshamilton.frontend.IntegerType
import eu.jameshamilton.frontend.LabeledStatement
import eu.jameshamilton.frontend.LongType
import eu.jameshamilton.frontend.NullStatement
import eu.jameshamilton.frontend.PointerType
import eu.jameshamilton.frontend.Program
import eu.jameshamilton.frontend.ReturnStatement
import eu.jameshamilton.frontend.SingleInit
import eu.jameshamilton.frontend.StringConstant
import eu.jameshamilton.frontend.Subscript
import eu.jameshamilton.frontend.Switch
import eu.jameshamilton.frontend.SwitchCase
import eu.jameshamilton.frontend.Type
import eu.jameshamilton.frontend.UIntType
import eu.jameshamilton.frontend.ULongType
import eu.jameshamilton.frontend.UnaryExpr
import eu.jameshamilton.frontend.UnaryOp
import eu.jameshamilton.frontend.UnaryOp.PostfixDecrement
import eu.jameshamilton.frontend.UnaryOp.PostfixIncrement
import eu.jameshamilton.frontend.UnaryOp.PrefixDecrement
import eu.jameshamilton.frontend.UnaryOp.PrefixIncrement
import eu.jameshamilton.frontend.Var
import eu.jameshamilton.frontend.VarDeclaration
import eu.jameshamilton.frontend.While
import eu.jameshamilton.frontend.check.FunAttr
import eu.jameshamilton.frontend.check.Initial
import eu.jameshamilton.frontend.check.LocalAttr
import eu.jameshamilton.frontend.check.NoInitializer
import eu.jameshamilton.frontend.check.StaticAttr
import eu.jameshamilton.frontend.check.SymbolTableEntry
import eu.jameshamilton.frontend.check.Tentative
import eu.jameshamilton.frontend.check.ZeroInit
import eu.jameshamilton.frontend.check.makestringconstant
import eu.jameshamilton.frontend.check.resolveSwitchCases
import eu.jameshamilton.frontend.check.symbolTable
import eu.jameshamilton.frontend.isSigned
import eu.jameshamilton.frontend.isUnsigned
import eu.jameshamilton.frontend.pointerTo
import eu.jameshamilton.unreachable
import eu.jameshamilton.tacky.BinaryOp as TackyBinaryOp
import eu.jameshamilton.tacky.Constant as TackyConstant
import eu.jameshamilton.tacky.FunctionDef as TackyFunctionDef
import eu.jameshamilton.tacky.Program as TackyProgram
import eu.jameshamilton.tacky.UnaryOp as TackyUnaryOp
import eu.jameshamilton.tacky.Var as TackyVar

fun convert(program: Program): TackyProgram {
    val functions =
        program.declarations
            .filterIsInstance<FunDeclaration>()
            .filter { it.body != null }
            .map { convert(it) }

    val staticVariables: List<StaticVariable> =
        symbolTable
            .filterValues { it.attr is StaticAttr }
            .map { Triple(it.key, it.value.type, it.value.attr as StaticAttr) }
            .mapNotNull { (name, type, attr) ->
                when (attr.initialValue) {
                    is Initial -> StaticVariable(name, attr.global, type, attr.initialValue.value)
                    Tentative -> StaticVariable(
                        name, attr.global, type, when (type) {
                            is IntType, is UIntType, is LongType, is ULongType, is DoubleType -> ZeroInit(type.sizeInBytes)
                            is ArrayType -> ZeroInit(type.sizeInBytes)
                            is PointerType -> ZeroInit(LongType.sizeInBytes)
                            else -> unreachable("invalid type $type")
                        }
                    )

                    NoInitializer -> null
                }
            }

    return TackyProgram(staticVariables + functions)
}

private var count = 0
private fun maketemporary(type: Type): TackyVar {
    val name = "tmp.${count++}"
    symbolTable[name] = SymbolTableEntry(type, LocalAttr)
    return TackyVar(type, name)
}

private var labels = 0
private fun makelabel(name: String): LabelIdentifier = "${name}_${labels++}"

sealed interface ExprResult {
    val value: Value
}

private data class PlainOperand(override val value: Value) : ExprResult
private data class DereferencedPointer(override val value: Value) : ExprResult

private fun convert(op: UnaryOp): TackyUnaryOp = when (op) {
    UnaryOp.Complement -> TackyUnaryOp.Complement
    UnaryOp.Negate -> TackyUnaryOp.Negate
    UnaryOp.Not -> TackyUnaryOp.Not
    PrefixIncrement, PostfixIncrement, PrefixDecrement, PostfixDecrement -> unreachable("special case")
}

private fun convert(operator: BinaryOp): TackyBinaryOp = when (operator) {
    Add -> TackyBinaryOp.Add
    Subtract -> TackyBinaryOp.Subtract
    Multiply -> TackyBinaryOp.Multiply
    Divide -> TackyBinaryOp.Divide
    Remainder -> TackyBinaryOp.Remainder
    And -> TackyBinaryOp.And
    Or -> TackyBinaryOp.Or
    Xor -> TackyBinaryOp.Xor
    LeftShift -> TackyBinaryOp.LeftShift
    RightShift -> TackyBinaryOp.RightShift
    LogicalAnd, LogicalOr -> unreachable("special case")
    Equal -> TackyBinaryOp.Equal
    NotEqual -> TackyBinaryOp.NotEqual
    LessThan -> TackyBinaryOp.LessThan
    GreaterThan -> TackyBinaryOp.GreaterThan
    LessThanOrEqual -> TackyBinaryOp.LessThanOrEqual
    GreaterThanOrEqual -> TackyBinaryOp.GreaterThanOrEqual
}

private fun convert(instructions: MutableList<Instruction>, type: Type, result: ExprResult): Value = when (result) {
    is DereferencedPointer -> {
        val dst = maketemporary(type)
        instructions += Load(result.value, dst)
        dst
    }

    is PlainOperand -> result.value
}

private fun emitAndConvert(instructions: MutableList<Instruction>, expression: Expression): Value =
    convert(instructions, expression.type, emit(instructions, expression))

private fun emit(instructions: MutableList<Instruction>, expression: Expression): ExprResult = when (expression) {
    is Constant -> PlainOperand(TackyConstant(expression.value))
    is StringConstant -> buildTacky(instructions) {
        val name = makestringconstant(expression.value)
        val dst = maketemporary(expression.type)
        getaddress(TackyVar(expression.type, name), dst)
        PlainOperand(dst)
    }
    is UnaryExpr -> buildTacky(instructions) {
        when (expression.op) {
            PostfixIncrement, PostfixDecrement -> {

                val amount = when {
                    expression.op == PostfixIncrement && expression.type is DoubleType -> 1.0
                    expression.op == PostfixDecrement && expression.type is IntegerType -> -1
                    expression.op == PostfixIncrement && expression.type is IntegerType -> 1
                    expression.op == PostfixDecrement && expression.type is DoubleType -> -1.0
                    expression.op == PostfixIncrement && expression.type is PointerType -> 1L
                    expression.op == PostfixDecrement && expression.type is PointerType -> -1L
                    else -> unreachable("Invalid expression: {${expression}}")
                }

                when (val value = emit(instructions, expression.expression)) {
                    is DereferencedPointer -> {
                        val dst = maketemporary(expression.type)
                        val tmp = convert(instructions, expression.type, value)
                        copy(tmp, dst)
                        increment(tmp, TackyConstant(amount))
                        store(tmp, value.value)
                        PlainOperand(dst)
                    }

                    is PlainOperand -> {
                        val dst = maketemporary(expression.type)
                        copy(value.value, dst)
                        if (expression.type is PointerType) {
                            addptr(
                                value.value,
                                TackyConstant(amount),
                                (expression.type as PointerType).referenced.sizeInBytes,
                                value.value
                            )
                        } else {
                            increment(value.value, TackyConstant(amount))
                        }
                        PlainOperand(dst)
                    }
                }
            }

            PrefixIncrement, PrefixDecrement -> {

                val amount = when {
                    expression.op == PrefixIncrement && expression.type is IntegerType -> 1
                    expression.op == PrefixDecrement && expression.type is IntegerType -> -1
                    expression.op == PrefixIncrement && expression.type is DoubleType -> 1.0
                    expression.op == PrefixDecrement && expression.type is DoubleType -> -1.0
                    expression.op == PrefixIncrement && expression.type is PointerType -> 1L
                    expression.op == PrefixDecrement && expression.type is PointerType -> -1L
                    else -> unreachable("Invalid expression: $expression")
                }

                when (val value = emit(instructions, expression.expression)) {
                    is DereferencedPointer -> {
                        val dst = convert(instructions, expression.type, value)
                        increment(dst, TackyConstant(amount))
                        store(dst, value.value)
                        PlainOperand(dst)
                    }

                    is PlainOperand -> {
                        if (expression.type is PointerType) {
                            addptr(
                                value.value,
                                TackyConstant(amount),
                                (expression.type as PointerType).referenced.sizeInBytes,
                                value.value
                            )
                        } else {
                            increment(value.value, TackyConstant(amount))
                        }
                        value
                    }
                }
            }

            else -> {
                val src = emitAndConvert(instructions, expression.expression)
                val dst = maketemporary(expression.type)
                val op = convert(expression.op)
                unaryOp(op, src, dst)
                PlainOperand(dst)
            }
        }
    }

    is BinaryExpr -> when (expression.operator) {
        LogicalAnd -> buildTacky(instructions) {
            val dst = maketemporary(expression.type)
            val falseLabel = makelabel("and_false_label")
            val endLabel = makelabel("and_end_label")

            val v1 = emitAndConvert(instructions, expression.left)
            jumpIfZero(v1, falseLabel)
            val v2 = emitAndConvert(instructions, expression.right)
            jumpIfZero(v2, falseLabel)
            copy(1, dst)
            jump(endLabel)
            label(falseLabel)
            copy(0, dst)
            label(endLabel)
            PlainOperand(dst)
        }

        LogicalOr -> buildTacky(instructions) {
            val dst = maketemporary(expression.type)
            val falseLabel = makelabel("or_false_label")
            val endLabel = makelabel("or_end_label")

            val v1 = emitAndConvert(instructions, expression.left)
            jumpIfNotZero(v1, falseLabel)
            val v2 = emitAndConvert(instructions, expression.right)
            jumpIfNotZero(v2, falseLabel)
            copy(0, dst)
            jump(endLabel)
            label(falseLabel)
            copy(1, dst)
            label(endLabel)
            PlainOperand(dst)
        }

        RightShift -> buildTacky(instructions) {
            val v1 = emitAndConvert(instructions, expression.left)
            val v2 = emitAndConvert(instructions, expression.right)
            val dst = maketemporary(expression.type)
            val leftType = expression.left.type
            val tackyOp = when (leftType is IntegerType && leftType.isSigned) {
                true -> TackyBinaryOp.RightShift
                false -> TackyBinaryOp.LogicalRightShift
            }
            binaryOp(tackyOp, v1, v2, dst)
            PlainOperand(dst)
        }

        Subtract -> buildTacky(instructions) {
            val v1 = emitAndConvert(instructions, expression.left)
            val v2 = emitAndConvert(instructions, expression.right)
            val dst = maketemporary(expression.type)

            when {
                expression.left.type is PointerType && expression.right.type is PointerType -> {
                    val ptrType = expression.left.type as PointerType
                    val diff = maketemporary(LongType)
                    sub(v1, v2, diff)
                    div(diff, TackyConstant(ptrType.referenced.sizeInBytes), dst)
                }

                expression.left.type is PointerType -> {
                    val ptrType = expression.left.type as PointerType
                    val tmp = maketemporary(expression.right.type)
                    addptr(v1, neg(v2, tmp), ptrType.referenced.sizeInBytes, dst)
                }

                expression.right.type is PointerType -> {
                    val ptrType = expression.right.type as PointerType
                    val tmp = maketemporary(expression.right.type)
                    addptr(v2, neg(v1, tmp), ptrType.referenced.sizeInBytes, dst)
                }

                else -> {
                    sub(v1, v2, dst)
                }
            }

            PlainOperand(dst)
        }

        Add -> buildTacky(instructions) {
            val v1 = emitAndConvert(instructions, expression.left)
            val v2 = emitAndConvert(instructions, expression.right)
            val dst = maketemporary(expression.type)

            when {
                expression.left.type is PointerType -> {
                    val ptr = expression.left.type as PointerType
                    addptr(v1, v2, ptr.referenced.sizeInBytes, dst)
                }

                expression.right.type is PointerType -> {
                    val ptr = expression.right.type as PointerType
                    addptr(v2, v1, ptr.referenced.sizeInBytes, dst)
                }

                else -> {
                    add(v1, v2, dst)
                }
            }

            PlainOperand(dst)
        }

        else -> buildTacky(instructions) {
            val v1 = emitAndConvert(instructions, expression.left)
            val v2 = emitAndConvert(instructions, expression.right)
            val dst = maketemporary(expression.type)
            val tackyOp = convert(expression.operator)
            binaryOp(tackyOp, v1, v2, dst)
            PlainOperand(dst)
        }
    }

    is Assignment -> {
        buildTacky(instructions) {
            // Don't accumulate any lvalue instructions for compound assignments,
            // as the lhs should only be evaluated once.
            // They will already be emitted and converted in the rvalue below.
            // e.g. *call() += 5; the function call should happen only once.
            val lvalue = emit(if (expression.compound) mutableListOf() else instructions, expression.lvalue)

            val rvalueInstructions = mutableListOf<Instruction>()
            val rvalue = emitAndConvert(rvalueInstructions, expression.rvalue)

            val dst = if (expression.compound && lvalue is DereferencedPointer) {
                // Fix-up the destination, since the lvalue destination
                // is not emitted in this case, so use the Load destination.
                rvalueInstructions.filterIsInstance<Load>().first().ptr
            } else {
                lvalue.value
            }

            instructions += rvalueInstructions

            when (lvalue) {
                is DereferencedPointer -> {
                    store(rvalue, dst)
                    PlainOperand(rvalue)
                }

                is PlainOperand -> {
                    copy(rvalue, dst)
                    lvalue
                }
            }
        }
    }

    is Var -> {
        val result = TackyVar(expression.type, expression.identifier.identifier)
        PlainOperand(result)
    }

    is Conditional -> buildTacky(instructions) {
        val result = maketemporary(expression.type)
        val condition = emitAndConvert(instructions, expression.condition)
        val elseLabel = makelabel("else_label")
        val endLabel = makelabel("end_label")
        jumpIfZero(condition, elseLabel)
        val e1 = emitAndConvert(instructions, expression.thenBranch)
        copy(e1, result)
        jump(endLabel)
        label(elseLabel)
        val e2 = emitAndConvert(instructions, expression.elseBranch)
        copy(e2, result)
        label(endLabel)
        PlainOperand(result)
    }

    is FunctionCall -> buildTacky(instructions) {
        val arguments = expression.arguments.map { emitAndConvert(instructions, it) }
        val result = maketemporary(expression.type)
        call(expression.identifier.identifier, arguments, result)
        PlainOperand(result)
    }

    is Cast -> buildTacky(instructions) {
        val result = emitAndConvert(instructions, expression.expression)
        if (expression.targetType == expression.expression.type) {
            PlainOperand(result)
        } else {
            val dst = maketemporary(expression.type)
            symbolTable[dst.name] = SymbolTableEntry(expression.targetType, LocalAttr)
            val targetType = expression.targetType
            val exprType = expression.expression.type
            when {
                exprType is IntegerType && exprType.isUnsigned && targetType is DoubleType -> uitod(result, dst)
                exprType is DoubleType && targetType is IntegerType && targetType.isUnsigned -> dtoui(result, dst)
                exprType is IntegerType && targetType is DoubleType -> itod(result, dst)
                exprType is DoubleType && targetType is IntegerType -> dtoi(result, dst)
                // if both types are the same size, it doesn't matter
                // if they are signed or unsigned when converted to assembly.
                targetType.sizeInBits == exprType.sizeInBits -> copy(result, dst)
                targetType.sizeInBits < exprType.sizeInBits -> truncate(result, dst)
                exprType is IntegerType && exprType.isSigned -> signextend(result, dst)
                else -> zeroextend(result, dst)
            }
            PlainOperand(dst)
        }
    }

    is AddrOf -> when (val value = emit(instructions, expression.expression)) {
        is DereferencedPointer -> PlainOperand(value.value)
        is PlainOperand -> buildTacky(instructions) {
            val dst = maketemporary(expression.type)
            getaddress(value.value, dst)
            PlainOperand(dst)
        }
    }

    is Dereference -> {
        val result = emitAndConvert(instructions, expression.expression)
        DereferencedPointer(result)
    }

    is Subscript -> buildTacky(instructions) {
        val expr1 = expression.expr1
        val expr2 = expression.expr2
        val v1 = emitAndConvert(instructions, expression.expr1)
        val v2 = emitAndConvert(instructions, expression.expr2)
        val dst = maketemporary(expression.type.pointerTo())

        when {
            expr1.type is PointerType && expr2.type is IntegerType -> {
                addptr(v1, v2, expression.type.sizeInBytes, dst)
            }

            expr2.type is PointerType && expr1.type is IntegerType -> {
                addptr(v2, v1, expression.type.sizeInBytes, dst)
            }

            else -> unreachable("already checked by the typechecker")
        }
        DereferencedPointer(dst)
    }

    is CompoundInit, is SingleInit -> unreachable("special case")
}

private fun convert(funDeclaration: FunDeclaration): TackyFunctionDef {

    val switches = resolveSwitchCases(funDeclaration)

    fun emitInitializer(instructions: MutableList<Instruction>, statement: VarDeclaration) {
        if (symbolTable[statement.name.identifier]?.attr !is LocalAttr || statement.initializer == null) {
            return
        }

        var offset = 0
        fun emit(initializer: Initializer) {
            buildTacky(instructions) {
                when {
                    initializer is CompoundInit -> initializer.expressions.forEach(::emit)
                    initializer is SingleInit && initializer.expression is StringConstant -> {
                        val type = statement.initializer.type as ArrayType
                        val string = initializer.expression.value
                        val dst = maketemporary(type)

                        (0 until type.length).forEach { index ->
                            if (index < string.length) {
                                copytooffset(TackyConstant(string[index]), dst, index)
                            } else {
                                copytooffset(TackyConstant(0.toChar()), dst, index)
                            }
                        }
                    }

                    initializer is SingleInit && statement.initializer.type is ArrayType -> {
                        val src = emitAndConvert(instructions, initializer.expression)
                        val dst = TackyVar(statement.initializer.type, statement.name.identifier)

                        copytooffset(src, dst, offset)
                        offset += initializer.expression.type.sizeInBytes
                    }

                    initializer is SingleInit -> {
                        val src = emitAndConvert(instructions, initializer.expression)
                        val dst = TackyVar(statement.initializer.type, statement.name.identifier)
                        copy(src, dst)
                    }

                    else -> unreachable("ERROR")
                }
                PlainOperand(nop())
            }
        }

        emit(statement.initializer)
    }

    fun emit(instructions: MutableList<Instruction>, statement: BlockItem) {
        buildTacky(instructions) {
            when (statement) {
                is ReturnStatement -> {
                    ret(emitAndConvert(instructions, statement.value))
                }

                is ExpressionStatement -> {
                    emit(instructions, statement.expression)
                }

                is NullStatement -> {
                    // Nothing to do here.
                }
                is VarDeclaration -> {
                    emitInitializer(instructions, statement)
                }

                is FunDeclaration -> {
                    if (statement.body != null) {
                        unreachable("local functions not supported")
                    }
                }

                is If -> {
                    val endLabel = makelabel("if_end")
                    val elseLabel = if (statement.elseBranch == null) endLabel else makelabel("else_label")
                    val condition = emitAndConvert(instructions, statement.condition)
                    jumpIfZero(condition, elseLabel)
                    emit(instructions, statement.thenBranch)
                    if (statement.elseBranch != null) {
                        jump(endLabel)
                        label(elseLabel)
                        emit(instructions, statement.elseBranch)
                    }
                    label(endLabel)
                }

                is Goto -> jump(statement.identifier.identifier)
                is LabeledStatement -> {
                    label(statement.identifier.identifier)
                    emit(instructions, statement.statement)
                }

                is Compound -> statement.block.forEach { emit(instructions, it) }
                is Break -> jump(statement.identifier!!.identifier)
                is Continue -> jump(statement.identifier!!.identifier)
                is DoWhile -> {
                    assert(statement.id != null)

                    val startLabel = makelabel("start")
                    val continueLabel = statement.continueLabel!!.identifier
                    val breakLabel = statement.breakLabel!!.identifier

                    label(startLabel)
                    emit(instructions, statement.body)
                    label(continueLabel)
                    val result = emitAndConvert(instructions, statement.condition)
                    jumpIfNotZero(result, startLabel)
                    label(breakLabel)
                }

                is While -> {
                    assert(statement.id != null)

                    val continueLabel = statement.continueLabel!!.identifier
                    val breakLabel = statement.breakLabel!!.identifier

                    label(continueLabel)
                    val result = emitAndConvert(instructions, statement.condition)
                    jumpIfZero(result, breakLabel)
                    emit(instructions, statement.body)
                    jump(continueLabel)
                    label(breakLabel)
                }

                is For -> {
                    when (statement.init) {
                        is InitDecl -> emitInitializer(instructions, statement.init.declaration)
                        is InitExpr -> statement.init.expression?.let { emit(instructions, it) }
                    }
                    val startLabel = makelabel("start")
                    val continueLabel = statement.continueLabel!!.identifier
                    val breakLabel = statement.breakLabel!!.identifier

                    label(startLabel)
                    if (statement.condition != null) {
                        val result = emitAndConvert(instructions, statement.condition)
                        jumpIfZero(result, breakLabel)
                    }
                    emit(instructions, statement.body)
                    label(continueLabel)
                    statement.post?.let { emit(instructions, it) }
                    jump(startLabel)
                    label(breakLabel)
                }

                is Switch -> {
                    require(statement.id != null)
                    require(switches.containsKey(statement))

                    val switchValue = emitAndConvert(instructions, statement.expression)
                    val cases = switches[statement]

                    require(cases != null)

                    if (cases.isNotEmpty()) {
                        require(statement.breakLabel != null)

                        val breakLabel = statement.breakLabel!!.identifier
                        val temp = maketemporary(statement.expression.type)

                        cases.forEach { case ->
                            require(case.label != null)
                            val target = case.label!!.identifier

                            when (case) {
                                is ExpressionCase -> {
                                    val caseValue = emitAndConvert(instructions, case.expression)
                                    equal(switchValue, caseValue, temp)
                                    jumpIfNotZero(temp, target)
                                }

                                is DefaultCase -> {
                                    jump(target)
                                }
                            }
                        }

                        if (cases.count { it is DefaultCase } == 0) {
                            jump(breakLabel)
                        }

                        emit(instructions, statement.statement)

                        label(breakLabel)
                    }
                    nop()
                }

                is SwitchCase -> {
                    label(statement.label!!.identifier)
                    emit(instructions, statement.statement)
                    nop()
                }
            }
            PlainOperand(nop())
        }
    }

    fun convert(statements: List<BlockItem>?): List<Instruction> = statements?.flatMap { blockItem ->
        val instructions = mutableListOf<Instruction>()
        emit(instructions, blockItem)
        instructions
    } ?: emptyList()

    val attr = symbolTable[funDeclaration.name.identifier]?.attr as FunAttr

    return TackyFunctionDef(
        funDeclaration.type,
        funDeclaration.name.identifier,
        attr.global,
        funDeclaration.body != null,
        funDeclaration.params?.map { it.name.identifier } ?: emptyList(),
        // TODO: don't add return for declarations?
        convert(funDeclaration.body) + listOf(Return(TackyConstant(0)))
    )
}
