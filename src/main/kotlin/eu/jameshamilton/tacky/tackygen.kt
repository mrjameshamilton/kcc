package eu.jameshamilton.tacky

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
import eu.jameshamilton.frontend.Compound
import eu.jameshamilton.frontend.Conditional
import eu.jameshamilton.frontend.Constant
import eu.jameshamilton.frontend.Continue
import eu.jameshamilton.frontend.DefaultCase
import eu.jameshamilton.frontend.DoWhile
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
import eu.jameshamilton.frontend.LabeledStatement
import eu.jameshamilton.frontend.NullStatement
import eu.jameshamilton.frontend.Program
import eu.jameshamilton.frontend.ReturnStatement
import eu.jameshamilton.frontend.Switch
import eu.jameshamilton.frontend.SwitchCase
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
import eu.jameshamilton.frontend.check.Tentative
import eu.jameshamilton.frontend.check.resolveSwitchCases
import eu.jameshamilton.frontend.check.symbolTable
import eu.jameshamilton.unreachable
import eu.jameshamilton.tacky.Binary as TackyBinary
import eu.jameshamilton.tacky.BinaryOp as TackyBinaryOp
import eu.jameshamilton.tacky.BinaryOp.Add as TackyBinaryOpAdd
import eu.jameshamilton.tacky.Constant as TackyConstant
import eu.jameshamilton.tacky.FunctionCall as TackyFunctionCall
import eu.jameshamilton.tacky.FunctionDef as TackyFunctionDef
import eu.jameshamilton.tacky.Label as TackyLabel
import eu.jameshamilton.tacky.Program as TackyProgram
import eu.jameshamilton.tacky.Unary as TackyUnary
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
            .map { (name, entry) -> name to (entry.attr as StaticAttr) }
            .mapNotNull { (name, staticAttr) ->
                when (staticAttr.initialValue) {
                    is Initial -> StaticVariable(name.identifier, staticAttr.global, staticAttr.initialValue.value)
                    Tentative -> StaticVariable(name.identifier, staticAttr.global, 0)
                    NoInitializer -> null
                }
            }

    return TackyProgram(staticVariables + functions)
}

private fun convert(funDeclaration: FunDeclaration): TackyFunctionDef {

    fun convert(op: UnaryOp): TackyUnaryOp = when (op) {
        UnaryOp.Complement -> TackyUnaryOp.Complement
        UnaryOp.Negate -> TackyUnaryOp.Negate
        UnaryOp.Not -> TackyUnaryOp.Not
        PrefixIncrement, PostfixIncrement, PrefixDecrement, PostfixDecrement -> unreachable("special case")
    }

    var count = 0
    fun maketemporary(): String = "tmp.${count++}"
    var labels = 0
    fun makelabel(name: String): LabelIdentifier = "${name}_${labels++}"

    val switches = resolveSwitchCases(funDeclaration)

    fun convert(operator: BinaryOp): TackyBinaryOp = when (operator) {
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

    fun convert(instructions: MutableList<Instruction>, expression: Expression): Value = when (expression) {
        is Constant -> TackyConstant(expression.value)
        is UnaryExpr -> buildTacky(instructions) {
            val src = convert(instructions, expression.expression)
            when (expression.op) {
                PostfixIncrement -> {
                    val dst = TackyVar(maketemporary())
                    copy(src, dst)
                    increment(src)
                    dst
                }

                PostfixDecrement -> {
                    val dst = TackyVar(maketemporary())
                    copy(src, dst)
                    increment(src, -1)
                    dst
                }

                PrefixIncrement -> increment(src)
                PrefixDecrement -> increment(src, -1)
                else -> {
                    val dst = TackyVar(maketemporary())
                    val op = convert(expression.op)
                    unaryOp(op, src, dst)
                }
            }
        }

        is BinaryExpr -> when (expression.operator) {
            LogicalAnd -> buildTacky(instructions) {
                val dst = TackyVar(maketemporary())
                val falseLabel = makelabel("and_false_label")
                val endLabel = makelabel("and_end_label")

                val v1 = convert(instructions, expression.left)
                jumpIfZero(v1, falseLabel)
                val v2 = convert(instructions, expression.right)
                jumpIfZero(v2, falseLabel)
                copy(1, dst)
                jump(endLabel)
                label(falseLabel)
                copy(0, dst)
                label(endLabel)
                dst
            }

            LogicalOr -> buildTacky(instructions) {
                val dst = TackyVar(maketemporary())
                val falseLabel = makelabel("or_false_label")
                val endLabel = makelabel("or_end_label")

                val v1 = convert(instructions, expression.left)
                jumpIfNotZero(v1, falseLabel)
                val v2 = convert(instructions, expression.right)
                jumpIfNotZero(v2, falseLabel)
                copy(0, dst)
                jump(endLabel)
                label(falseLabel)
                copy(1, dst)
                label(endLabel)
                dst
            }

            else -> buildTacky(instructions) {
                val v1 = convert(instructions, expression.left)
                val v2 = convert(instructions, expression.right)
                val dst = TackyVar(maketemporary())
                val tackyOp = convert(expression.operator)
                binaryOp(tackyOp, v1, v2, dst)
                dst
            }
        }

        is Assignment -> buildTacky(instructions) {
            val value = convert(instructions, expression.value)
            val lvalue = convert(instructions, expression.lvalue)
            copy(value, lvalue)
            value
        }

        is Var -> TackyVar(expression.identifier.identifier)
        is Conditional -> buildTacky(instructions) {
            val result = TackyVar(maketemporary())
            val condition = convert(instructions, expression.condition)
            val elseLabel = makelabel("else_label")
            val endLabel = makelabel("end_label")
            jumpIfZero(condition, elseLabel)
            val e1 = convert(instructions, expression.thenBranch)
            copy(e1, result)
            jump(endLabel)
            label(elseLabel)
            val e2 = convert(instructions, expression.elseBranch)
            copy(e2, result)
            label(endLabel)
            result
        }

        is FunctionCall -> buildTacky(instructions) {
            val arguments = expression.arguments.map { convert(instructions, it) }
            val result = TackyVar(maketemporary())
            call(expression.identifier.identifier, arguments, result)
            result
        }
    }

    fun convert(statement: BlockItem): List<Instruction> {
        val instructions = mutableListOf<Instruction>()
        buildTacky(instructions) {
            when (statement) {
                is ReturnStatement -> ret(convert(instructions, statement.value))
                is ExpressionStatement -> convert(instructions, statement.expression)
                is NullStatement -> emptyList<Instruction>()
                is VarDeclaration -> {
                    if (symbolTable[statement.name]?.attr is LocalAttr && statement.initializer != null) {
                        val src = convert(instructions, statement.initializer)
                        val dst = TackyVar(statement.name.identifier)
                        copy(src, dst)
                    }
                }

                is FunDeclaration -> {
                    if (statement.body != null) {
                        unreachable("local functions not supported")
                    }
                }

                is If -> {
                    val endLabel = makelabel("if_end")
                    val elseLabel = if (statement.elseBranch == null) endLabel else makelabel("else_label")
                    val condition = convert(instructions, statement.condition)
                    jumpIfZero(condition, elseLabel)
                    instructions += convert(statement.thenBranch)
                    if (statement.elseBranch != null) {
                        jump(endLabel)
                        label(elseLabel)
                        instructions += convert(statement.elseBranch)
                    }
                    label(endLabel)
                }

                is Goto -> jump(statement.identifier.identifier)
                is LabeledStatement -> {
                    label(statement.identifier.identifier)
                    instructions += convert(statement.statement)
                }

                // TODO: refactor instructions += for statements?
                is Compound -> instructions += statement.block.flatMap { convert(it) }
                is Break -> jump(statement.identifier!!.identifier)
                is Continue -> jump(statement.identifier!!.identifier)
                is DoWhile -> buildTacky(instructions) {
                    assert(statement.id != null)

                    val startLabel = makelabel("start")
                    val continueLabel = statement.continueLabel!!.identifier
                    val breakLabel = statement.breakLabel!!.identifier

                    label(startLabel)
                    instructions += convert(statement.body)
                    label(continueLabel)
                    val result = convert(instructions, statement.condition)
                    jumpIfNotZero(result, startLabel)
                    label(breakLabel)
                }

                is While -> buildTacky(instructions) {
                    assert(statement.id != null)

                    val continueLabel = statement.continueLabel!!.identifier
                    val breakLabel = statement.breakLabel!!.identifier

                    label(continueLabel)
                    val result = convert(instructions, statement.condition)
                    jumpIfZero(result, breakLabel)
                    instructions += convert(statement.body)
                    jump(continueLabel)
                    label(breakLabel)
                }

                is For -> buildTacky(instructions) {
                    when (statement.init) {
                        is InitDecl -> instructions += convert(statement.init.declaration)
                        is InitExpr -> statement.init.expression?.let { convert(instructions, it) }
                    }
                    val startLabel = makelabel("start")
                    val continueLabel = statement.continueLabel!!.identifier
                    val breakLabel = statement.breakLabel!!.identifier

                    label(startLabel)
                    if (statement.condition != null) {
                        val result = convert(instructions, statement.condition)
                        jumpIfZero(result, breakLabel)
                    }
                    instructions += convert(statement.body)
                    label(continueLabel)
                    statement.post?.let { convert(instructions, it) }
                    jump(startLabel)
                    label(breakLabel)
                }

                is Switch -> buildTacky(instructions) {
                    require(statement.id != null)
                    require(switches.containsKey(statement))

                    val switchValue = convert(instructions, statement.expression)
                    val cases = switches[statement]

                    require(cases != null)

                    if (cases.isNotEmpty()) {
                        require(statement.breakLabel != null)

                        val breakLabel = statement.breakLabel!!.identifier
                        val temp = TackyVar(maketemporary())

                        cases.forEach { case ->
                            require(case.label != null)
                            val target = case.label!!.identifier

                            when (case) {
                                is ExpressionCase -> {
                                    val caseValue = convert(instructions, case.expression)
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

                        instructions += convert(statement.statement)

                        label(breakLabel)
                    }
                    nop()
                }

                is SwitchCase -> buildTacky(instructions) {
                    label(statement.label!!.identifier)
                    instructions += convert(statement.statement)
                    nop()
                }
            }
            nop()
        }
        return instructions
    }

    fun convert(statements: List<BlockItem>?): List<Instruction> {
        return statements?.flatMap { convert(it) } ?: emptyList()
    }

    val attr = symbolTable[funDeclaration.name]?.attr as FunAttr

    return TackyFunctionDef(
        funDeclaration.name.identifier,
        attr.global,
        funDeclaration.params?.map { it.name.identifier } ?: emptyList(),
        convert(funDeclaration.body) + listOf(TackyReturn(TackyConstant(0)))
    )
}

class Builder(private val instructions: MutableList<Instruction> = mutableListOf()) {
    fun jump(target: String): Value {
        instructions += Jump(target)
        return nop()
    }

    fun jumpIfZero(condition: Value, target: String) {
        instructions += JumpIfZero(condition, target)
    }

    fun jumpIfNotZero(condition: Value, target: String) {
        instructions += JumpIfNotZero(condition, target)
    }

    fun label(label: LabelIdentifier): Value {
        instructions += TackyLabel(label)
        return nop()
    }

    fun copy(src: Value, dst: Value): Value {
        instructions += Copy(src, dst)
        return dst
    }

    fun copy(i: Int, dst: Value): Value {
        instructions += Copy(TackyConstant(i), dst)
        return dst
    }

    fun binaryOp(binaryOp: TackyBinaryOp, src1: Value, src2: Value, dst: Value): Value {
        instructions += TackyBinary(binaryOp, src1, src2, dst)
        return dst
    }

    fun equal(src1: Value, src2: Value, dst: Value): Value {
        return binaryOp(TackyBinaryOp.Equal, src1, src2, dst)
    }

    fun increment(src: Value, amount: Int = 1): Value {
        return increment(src, TackyConstant(amount))
    }

    fun increment(src: Value, amount: Value): Value {
        instructions += TackyBinary(TackyBinaryOpAdd, src, amount, src)
        return src
    }

    fun unaryOp(unaryOp: TackyUnaryOp, src: Value, dst: Value): Value {
        instructions += TackyUnary(unaryOp, src, dst)
        return dst
    }

    fun ret(value: Value) {
        instructions += TackyReturn(value)
    }

    fun call(identifier: String, arguments: List<Value>, result: Value): Value {
        instructions += TackyFunctionCall(identifier, arguments, result)
        return result
    }

    fun nop(): Value = TackyConstant(0)
}

fun buildTacky(instructions: MutableList<Instruction>, block: Builder.() -> Value): Value =
    with(Builder(instructions)) {
        return block(this)
    }
