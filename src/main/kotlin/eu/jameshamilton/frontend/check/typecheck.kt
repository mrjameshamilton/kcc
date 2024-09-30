package eu.jameshamilton.frontend.check

import eu.jameshamilton.frontend.AddrOf
import eu.jameshamilton.frontend.ArithmeticType
import eu.jameshamilton.frontend.ArrayType
import eu.jameshamilton.frontend.Assignment
import eu.jameshamilton.frontend.BinaryExpr
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
import eu.jameshamilton.frontend.CharType
import eu.jameshamilton.frontend.CharacterType
import eu.jameshamilton.frontend.Compound
import eu.jameshamilton.frontend.CompoundInit
import eu.jameshamilton.frontend.Conditional
import eu.jameshamilton.frontend.Constant
import eu.jameshamilton.frontend.Continue
import eu.jameshamilton.frontend.Declaration
import eu.jameshamilton.frontend.DefaultCase
import eu.jameshamilton.frontend.Dereference
import eu.jameshamilton.frontend.DoWhile
import eu.jameshamilton.frontend.DoubleType
import eu.jameshamilton.frontend.Expression
import eu.jameshamilton.frontend.ExpressionCase
import eu.jameshamilton.frontend.ExpressionStatement
import eu.jameshamilton.frontend.For
import eu.jameshamilton.frontend.ForInit
import eu.jameshamilton.frontend.FunDeclaration
import eu.jameshamilton.frontend.FunType
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
import eu.jameshamilton.frontend.SCharType
import eu.jameshamilton.frontend.SingleInit
import eu.jameshamilton.frontend.Statement
import eu.jameshamilton.frontend.StorageClass.EXTERN
import eu.jameshamilton.frontend.StorageClass.NONE
import eu.jameshamilton.frontend.StorageClass.STATIC
import eu.jameshamilton.frontend.StringConstant
import eu.jameshamilton.frontend.Subscript
import eu.jameshamilton.frontend.Switch
import eu.jameshamilton.frontend.Type
import eu.jameshamilton.frontend.UCharType
import eu.jameshamilton.frontend.UIntType
import eu.jameshamilton.frontend.ULongType
import eu.jameshamilton.frontend.UnaryExpr
import eu.jameshamilton.frontend.UnaryOp.Complement
import eu.jameshamilton.frontend.UnaryOp.Negate
import eu.jameshamilton.frontend.UnaryOp.Not
import eu.jameshamilton.frontend.UnaryOp.PostfixDecrement
import eu.jameshamilton.frontend.UnaryOp.PostfixIncrement
import eu.jameshamilton.frontend.UnaryOp.PrefixDecrement
import eu.jameshamilton.frontend.UnaryOp.PrefixIncrement
import eu.jameshamilton.frontend.Unknown
import eu.jameshamilton.frontend.Var
import eu.jameshamilton.frontend.VarDeclaration
import eu.jameshamilton.frontend.VoidType
import eu.jameshamilton.frontend.While
import eu.jameshamilton.frontend.cast
import eu.jameshamilton.frontend.castForAssignment
import eu.jameshamilton.frontend.commonPointerType
import eu.jameshamilton.frontend.commonType
import eu.jameshamilton.frontend.error
import eu.jameshamilton.frontend.isLValue
import eu.jameshamilton.frontend.plus
import eu.jameshamilton.isNegativeZero
import eu.jameshamilton.unreachable
import java.util.*

typealias SymbolTable = HashMap<String, SymbolTableEntry>

data class SymbolTableEntry(val type: Type, val attr: IdentifierAttr? = null)

val symbolTable = SymbolTable()

sealed class IdentifierAttr
data class FunAttr(val defined: Boolean, val global: Boolean) : IdentifierAttr()
data class StaticAttr(val initialValue: InitialValue, val global: Boolean) : IdentifierAttr()
data class ConstantAttr<T>(val staticInit: StaticInit<T>) : IdentifierAttr()
data object LocalAttr : IdentifierAttr()
sealed class InitialValue
data object Tentative : InitialValue()
data class Initial(val value: List<StaticInit<*>>) : InitialValue() {
    constructor(vararg value: StaticInit<*>) : this(value.toList())
}

data object NoInitializer : InitialValue()

sealed class StaticInit<T>(open val value: T)
data class IntInit(override val value: Int) : StaticInit<Int>(value)
data class CharInit(override val value: Byte) : StaticInit<Byte>(value)
data class UCharInit(override val value: UByte) : StaticInit<UByte>(value)
data class LongInit(override val value: Long) : StaticInit<Long>(value)
data class UIntInit(override val value: UInt) : StaticInit<UInt>(value)
data class ULongInit(override val value: ULong) : StaticInit<ULong>(value)
data class DoubleInit(override val value: Double) : StaticInit<Double>(value)
data class ZeroInit(val bytes: Int) : StaticInit<Int>(0)
data class StringInit(override val value: String, val isNullTerminated: Boolean) : StaticInit<String>(value)
data class PointerInit(override val value: String) : StaticInit<String>(value)

fun makestringconstant(value: String): String {
    val count =
        symbolTable.count { it.value.attr is ConstantAttr<*> && (it.value.attr as ConstantAttr<*>).staticInit is StringInit }
    val name = "string.${count + 1}"
    symbolTable[name] = SymbolTableEntry(
        ArrayType(CharType, value.length + 1),
        ConstantAttr(StringInit(value, true))
    )
    return name
}

fun typecheck(program: Program): Program {
    return Program(program.declarations.map {
        checkfilescope(it)
    })
}

private val switchType = Stack<Type>()

private fun checkfilescope(declaration: Declaration): Declaration = when (declaration) {
    is FunDeclaration -> typecheck(declaration)
    is VarDeclaration -> checkfilescope(declaration)
}

private fun typecheck(funDeclaration: FunDeclaration): FunDeclaration {
    if (funDeclaration.type.returnType is ArrayType) {
        error(funDeclaration.name.line, "Function '${funDeclaration.name}' cannot return an array type.")
    }

    val paramsTypes = funDeclaration.params?.map {
        when (val type = it.type) {
            is ArrayType -> PointerType(type.element)
            else -> type
        }
    }.orEmpty()
    val type = FunType(paramsTypes, funDeclaration.type.returnType)

    var alreadyDefined = false
    // static functions will not be globally visible.
    var global = funDeclaration.storageClass != STATIC

    if (funDeclaration.name.identifier in symbolTable) {
        val oldDeclaration = symbolTable[funDeclaration.name.identifier]!!

        if (oldDeclaration.type !is FunType) {
            error(
                funDeclaration.name.line,
                "Function '${funDeclaration.name}' previously defined as non-function type '${oldDeclaration.type}'."
            )
        }

        if (oldDeclaration.type != type) {
            error(
                funDeclaration.name.line,
                "Function '${funDeclaration.name}${funDeclaration.type}' previously defined with different type '${oldDeclaration.type}'."
            )
        }

        val oldAttr = oldDeclaration.attr
        require(oldAttr is FunAttr)

        alreadyDefined = oldAttr.defined
        if (alreadyDefined && funDeclaration.body != null) {
            error(
                funDeclaration.name.line,
                "Function '${funDeclaration.name.identifier}' is defined more than once."
            )
        }

        if (oldAttr.global && funDeclaration.storageClass == STATIC) {
            error(
                funDeclaration.name.line,
                "Function '${funDeclaration.name}' previously declared as non-static."
            )
        }

        global = oldAttr.global
    }

    val attr = FunAttr(alreadyDefined || funDeclaration.body != null, global)
    symbolTable[funDeclaration.name.identifier] = SymbolTableEntry(type, attr)

    val params = funDeclaration.params?.zip(paramsTypes)?.map { (varDeclaration, type) ->
        VarDeclaration(varDeclaration.name, varDeclaration.initializer, type, varDeclaration.storageClass)
    }
    if (funDeclaration.body != null) {
        params?.forEach {
            symbolTable[it.name.identifier] = SymbolTableEntry(it.type, LocalAttr)
        }
    }

    val body = funDeclaration.body?.map { typecheck(funDeclaration, it) }
    return FunDeclaration(
        funDeclaration.name,
        params,
        body,
        type,
        funDeclaration.storageClass
    )
}

private fun checklocalscope(varDeclaration: VarDeclaration): VarDeclaration {
    var initializer: Initializer? = null
    when (varDeclaration.storageClass) {
        EXTERN -> {
            if (varDeclaration.initializer != null) {
                error(
                    varDeclaration.name.line,
                    "Local variable '${varDeclaration.name}' with 'extern' storage cannot have initializer."
                )
            }

            if (varDeclaration.name.identifier in symbolTable) {
                val oldDeclaration = symbolTable[varDeclaration.name.identifier]!!
                if (oldDeclaration.type != varDeclaration.type) {
                    error(
                        varDeclaration.name.line,
                        "Variable '${varDeclaration.name}' previously defined as ${oldDeclaration.type}."
                    )
                }
            } else {
                val attrs = StaticAttr(NoInitializer, true)
                symbolTable[varDeclaration.name.identifier] = SymbolTableEntry(varDeclaration.type, attrs)
            }
        }

        STATIC -> {
            val initialValue = when (varDeclaration.initializer) {
                null -> Initial(ZeroInit(varDeclaration.type.sizeInBytes))
                else -> Initial(convertStaticInitializer(varDeclaration.type, varDeclaration.initializer))
            }

            val attrs = StaticAttr(initialValue, false)
            symbolTable[varDeclaration.name.identifier] = SymbolTableEntry(varDeclaration.type, attrs)
        }

        NONE -> {
            symbolTable[varDeclaration.name.identifier] = SymbolTableEntry(varDeclaration.type, LocalAttr)
            initializer =
                varDeclaration.initializer?.let { typecheckInit(varDeclaration.type, varDeclaration.initializer) }
        }
    }

    return VarDeclaration(
        varDeclaration.name,
        initializer,//.castForAssignment(varDeclaration.type)?.let { SingleInit(it, varDeclaration.type) },
        varDeclaration.type,
        varDeclaration.storageClass
    )
}

private fun checkfilescope(varDeclaration: VarDeclaration): VarDeclaration {
    // all variables declared at file static have static storage duration.
    var initialValue = when (varDeclaration.initializer) {
        is Initializer -> {
            Initial(convertStaticInitializer(varDeclaration.type, varDeclaration.initializer))
        }

        null -> when (varDeclaration.storageClass) {
            EXTERN -> NoInitializer
            else -> Tentative
        }
    }

    var global = varDeclaration.storageClass != STATIC

    if (varDeclaration.name.identifier in symbolTable) {
        val oldDeclaration = symbolTable[varDeclaration.name.identifier]!!

        if (oldDeclaration.type != varDeclaration.type) {
            error(
                varDeclaration.name.line,
                "Variable '${varDeclaration.name}' previously defined as '${varDeclaration.type}'."
            )
        }

        if (oldDeclaration.attr is StaticAttr) {
            if (varDeclaration.storageClass == EXTERN) {
                global = oldDeclaration.attr.global
            } else if (oldDeclaration.attr.global != global) {
                error(varDeclaration.name.line, "Variable '${varDeclaration.name}' has conflicting linkage.")
            }

            if (oldDeclaration.attr.initialValue is Initial) {
                if (initialValue is Initial) {
                    error(
                        varDeclaration.name.line,
                        "Multiple declarations of '${varDeclaration.name.identifier}' have initializers."
                    )
                } else {
                    initialValue = oldDeclaration.attr.initialValue
                }
            } else if (initialValue !is Initial && oldDeclaration.attr.initialValue is Tentative) {
                initialValue = Tentative
            }
        }
    }

    val attrs = StaticAttr(initialValue, global)
    symbolTable[varDeclaration.name.identifier] = SymbolTableEntry(varDeclaration.type, attrs)

    return VarDeclaration(
        varDeclaration.name,
        varDeclaration.initializer,
        varDeclaration.type,
        varDeclaration.storageClass
    )
}

private fun typecheck(expression: Expression): Expression = when (expression) {
    is Assignment -> {
        val left = typecheckAndConvert(expression.lvalue)

        if (!left.isLValue) {
            error(0, "Expression is not an lvalue.")
        }

        val right = typecheckAndConvert(expression.rvalue)
        Assignment(left, right.castForAssignment(left.type), expression.compound, left.type)
    }

    is BinaryExpr -> {
        val left = typecheckAndConvert(expression.left)
        val right = typecheckAndConvert(expression.right)

        val commonType = when {
            expression.operator in setOf(
                Equal,
                NotEqual
            ) && (left.type is PointerType || right.type is PointerType) -> left commonPointerType right

            else -> left.type + right.type
        }

        if (commonType is DoubleType && expression.operator in setOf(Remainder, LeftShift, RightShift, Xor, And, Or)) {
            error("'${expression.operator}' operator cannot be applied to '${commonType}' types.")
        }

        if ((left.type is PointerType || right.type is PointerType) && expression.operator in setOf(
                Multiply,
                Divide,
                Remainder,
                Xor,
                Or,
                And,
                LeftShift,
                RightShift
            )
        ) {
            error("'${expression.operator}' operator cannot be applied to 'pointer' types.")
        }

        when (expression.operator) {
            LogicalAnd, LogicalOr -> {
                BinaryExpr(left, expression.operator, right, IntType)
            }

            LeftShift, RightShift -> {
                if (left.type is CharacterType) {
                    // promote character to integer type.
                    BinaryExpr(left.cast(commonType), expression.operator, right.cast(commonType), IntType)
                } else {
                    BinaryExpr(left, expression.operator, right, left.type)
                }
            }

            And, Or, Xor -> {
                BinaryExpr(left.cast(commonType), expression.operator, right.cast(commonType), commonType)
            }

            Add -> {
                when {
                    left.type is ArithmeticType && right.type is ArithmeticType -> {
                        BinaryExpr(left.cast(commonType), expression.operator, right.cast(commonType), commonType)
                    }

                    left.type is PointerType && right.type is IntegerType -> {
                        BinaryExpr(left, expression.operator, right.cast(LongType), left.type)
                    }

                    left.type is IntegerType && right.type is PointerType -> {
                        BinaryExpr(left.cast(LongType), expression.operator, right, right.type)
                    }

                    left.type is CharacterType || right.type is CharacterType -> {
                        BinaryExpr(left.cast(commonType), expression.operator, right.cast(commonType), commonType)
                    }

                    else -> {
                        error("Invalid operands for '${expression.operator}': ${left.type} ${expression.operator} ${right.type}")
                    }
                }
            }

            Subtract -> {
                when {
                    left.type is ArithmeticType && right.type is ArithmeticType -> {
                        BinaryExpr(left.cast(commonType), expression.operator, right.cast(commonType), commonType)
                    }

                    left.type is PointerType && right.type is IntegerType -> {
                        BinaryExpr(left, expression.operator, right.cast(LongType), left.type)
                    }

                    left.type is PointerType && left.type == right.type -> {
                        BinaryExpr(left, expression.operator, right, LongType)
                    }

                    left.type is CharacterType || right.type is CharacterType -> {
                        BinaryExpr(left.cast(commonType), expression.operator, right.cast(commonType), commonType)
                    }

                    else -> {
                        error("Invalid operands for '${expression.operator}': ${left.type} ${expression.operator} ${right.type}")
                    }
                }
            }

            Multiply, Divide, Remainder -> {
                when {
                    left.type is ArithmeticType && right.type is ArithmeticType -> {
                        BinaryExpr(left.cast(commonType), expression.operator, right.cast(commonType), commonType)
                    }

                    else -> {
                        BinaryExpr(left.cast(commonType), expression.operator, right.cast(commonType), IntType)
                    }
                }
            }

            Equal, NotEqual -> {
                BinaryExpr(left.cast(commonType), expression.operator, right.cast(commonType), IntType)
            }

            LessThan, GreaterThan, LessThanOrEqual, GreaterThanOrEqual -> when {
                left.type is CharacterType || right.type is CharacterType -> {
                    BinaryExpr(left.cast(commonType), expression.operator, right.cast(commonType), IntType)
                }

                left.type is ArithmeticType && right.type is ArithmeticType -> {
                    BinaryExpr(left.cast(commonType), expression.operator, right.cast(commonType), IntType)
                }

                left.type is PointerType && left.type == right.type -> {
                    BinaryExpr(left, expression.operator, right, IntType)
                }

                else -> {
                    error("Invalid operands for '${expression.operator}': ${left.type} ${expression.operator} ${right.type}")
                }
            }
        }
    }

    is Conditional -> {
        val condition = typecheckAndConvert(expression.condition)
        val thenBranch = typecheckAndConvert(expression.thenBranch)
        val elseBranch = typecheckAndConvert(expression.elseBranch)

        val commonType = thenBranch commonType elseBranch

        Conditional(condition, thenBranch.cast(commonType), elseBranch.cast(commonType), commonType)
    }

    is Constant -> {
        val type = when (expression.value) {
            is Long -> LongType
            is Int -> IntType
            is ULong -> ULongType
            is UInt -> UIntType
            is Double -> DoubleType
            else -> error("Invalid type for ${expression.value}")
        }
        Constant(expression.value, type)
    }

    is StringConstant -> StringConstant(expression.value, ArrayType(CharType, expression.value.length + 1))

    is FunctionCall -> {
        val identifier = expression.identifier

        if (identifier.identifier !in symbolTable) {
            error(identifier.line, "'${identifier.identifier}' is not defined.")
        }

        val type = symbolTable[identifier.identifier]!!.type
        if (type !is FunType) {
            error(identifier.line, "'${identifier.identifier}' is not function.")
        }

        if (expression.arguments.size != type.paramsTypes.size) {
            error(
                identifier.line,
                "'${identifier.identifier}' called with wrong number of arguments (${expression.arguments.size} found, expected ${type.paramsTypes.size})."
            )
        }

        val typedArgs = expression.arguments
            .zip(type.paramsTypes)
            .map { (argExpr, paramType) -> typecheckAndConvert(argExpr).castForAssignment(paramType) }

        FunctionCall(expression.identifier, typedArgs, type.returnType)
    }

    is UnaryExpr -> {
        val expr = typecheckAndConvert(expression.expression)

        when (expression.op) {
            Complement -> if (expr.type is DoubleType || expr.type is PointerType) {
                error("'${expression.op}' operator cannot be applied to '${expr.type}' types.")
            }

            Negate -> if (expr.type is PointerType) {
                error("'${expression.op}' operator cannot be applied to '${expr.type}' types.")
            }

            PrefixIncrement, PrefixDecrement, PostfixIncrement, PostfixDecrement -> if (expr is AddrOf) {
                error("lvalue required as '${expression.op}' operand.")
            }

            else -> {}
        }

        when {
            expression.op == Not -> {
                UnaryExpr(expression.op, expr, IntType)
            }

            expression.op in setOf(Complement, Negate) && expr.type is CharacterType -> {
                UnaryExpr(expression.op, expr.cast(IntType), IntType)
            }

            else -> {
                UnaryExpr(expression.op, expr, expr.type)
            }
        }

    }

    is Var -> {
        val type = symbolTable[expression.identifier.identifier]?.type ?: Unknown

        if (type is FunType) {
            error(expression.identifier.line, "'${expression.identifier}' is a function used as a variable.")
        }

        Var(expression.identifier, type)
    }

    is Cast -> {
        val expr = typecheckAndConvert(expression.expression)

        if (expression.targetType is ArrayType) {
            error("Cannot cast to an array type.")
        }

        expr.cast(expression.targetType)
    }

    is AddrOf -> {
        if (!expression.expression.isLValue) {
            error(0, "Can only get the address of an assignable/lvalue; found '${expression.expression}'.")
        }
        val expr = typecheck(expression.expression)
        AddrOf(expr, PointerType(expr.type))
    }

    is Dereference -> {
        val expr = typecheckAndConvert(expression.expression)

        if (expr.type is PointerType) {
            Dereference(expr, (expr.type as PointerType).referenced)
        } else {
            error(0, "Can only dereference a pointer type; found '${expr.type}'.")
        }
    }

    is Initializer -> {
        typecheckInit(expression.type, expression)
    }

    is Subscript -> {
        val expr1 = typecheckAndConvert(expression.expr1)
        val expr2 = typecheckAndConvert(expression.expr2)

        when {
            expr1.type is PointerType && expr2.type is IntegerType -> {
                Subscript(expr1, expr2.cast(LongType), (expr1.type as PointerType).referenced)
            }

            expr2.type is PointerType && expr1.type is IntegerType -> {
                Subscript(expr1.cast(LongType), expr2, (expr2.type as PointerType).referenced)
            }

            else -> error("Invalid types for subscript: ${expr1.type} and ${expr2.type}.")
        }
    }
}

private fun typecheckInit(targetType: Type, init: Initializer): Initializer = when {
    init is SingleInit && targetType is ArrayType && init.expression is StringConstant -> {
        if (targetType.element !is CharacterType) {
            error("Can't initialize a non-character type with a string literal")
        }
        if (init.expression.value.length > targetType.length) {
            error("Too many characeters in string literal")
        }
        SingleInit(StringConstant(init.expression.value, targetType), targetType)
    }
    init is SingleInit -> {
        val expression = typecheckAndConvert(init.expression)
        SingleInit(expression.castForAssignment(targetType), targetType)
    }

    init is CompoundInit && targetType is ArrayType -> {
        if (init.expressions.size > targetType.length) {
            error("The number of values in the initializer does not match array dimensions.")
        }

        val initializers = init.expressions.map {
            typecheckInit(targetType.element, it)
        }
        val zeroinitializers = (initializers.size until targetType.length).map {
            zeroinit(targetType.element)
        }

        CompoundInit(initializers + zeroinitializers, targetType)
    }

    else -> error("Can't initialize a scalar object with a compound initializer.")
}

private fun zeroinit(targetType: Type): Initializer = when (targetType) {
    VoidType -> TODO()
    is ArrayType -> CompoundInit((0 until targetType.length).map { zeroinit(targetType.element) }, targetType)
    DoubleType -> SingleInit(Constant(0.0, targetType), targetType)
    IntType -> SingleInit(Constant(0, targetType), targetType)
    LongType -> SingleInit(Constant(0L, targetType), targetType)
    UIntType -> SingleInit(Constant(0U, targetType), targetType)
    ULongType -> SingleInit(Constant(0UL, targetType), targetType)
    is CharType, SCharType -> SingleInit(Constant(0.toByte(), targetType), targetType)
    is UCharType -> SingleInit(Constant(0.toUByte(), targetType), targetType)
    is PointerType -> SingleInit(Constant(0UL, targetType), targetType)
    Unknown, is FunType -> error("Invalid type for zeroinit: '${targetType}'.")
}

private fun convertStaticInitializer(targetType: Type, init: Initializer): List<StaticInit<*>> = when {
    init is SingleInit && targetType is ArrayType && init.expression is StringConstant -> {
        if (targetType.baseType !is CharacterType) {
            error("Can't initialize a non-character type with a string literal.")
        }
        val length = init.expression.value.length
        if (length > targetType.length) {
            error("Too many characeters in string literal")
        }

        val isNullTerminated = (length < targetType.length)
        val padding = if (targetType.length - length - 1 > 0) targetType.length - length - 1 else 0
        val stringInit = StringInit(init.expression.value, isNullTerminated)

        if (padding > 0) listOf(stringInit, ZeroInit(padding)) else listOf(stringInit)
    }

    init is SingleInit && init.expression is StringConstant && targetType is PointerType && targetType.referenced is CharacterType -> {
        if (targetType.referenced !is CharType) {
            error("Can't initialize $targetType type with a string literal.")
        }

        val name = makestringconstant(init.expression.value)
        listOf(PointerInit(name))
    }
    init is SingleInit -> {
        val expr = typecheckAndConvert(init.expression).castForAssignment(targetType)

        if (expr !is Constant) {
            error("Static variable must have constant initializer.")
        }

        val result = when (expr.value) {
            is Byte -> if (expr.value == 0.toByte()) ZeroInit(expr.type.sizeInBytes) else CharInit(expr.value)
            is UByte -> if (expr.value == 0.toUByte()) ZeroInit(expr.type.sizeInBytes) else UCharInit(expr.value)
            is Int -> if (expr.value == 0) ZeroInit(expr.type.sizeInBytes) else IntInit(expr.value)
            is UInt -> if (expr.value == 0u) ZeroInit(expr.type.sizeInBytes) else UIntInit(expr.value)
            is Long -> if (expr.value == 0L) ZeroInit(expr.type.sizeInBytes) else LongInit(expr.value)
            is ULong -> if (expr.value == 0uL) ZeroInit(expr.type.sizeInBytes) else ULongInit(expr.value)
            is Double -> {
                if (expr.value == 0.0 && !expr.value.isNegativeZero)
                    ZeroInit(expr.type.sizeInBytes)
                else
                    DoubleInit(expr.value)
            }

            else -> error("Invalid initializer value '${expr.value}'")
        }
        listOf(result)
    }

    init is CompoundInit && targetType is ArrayType -> {
        if (init.expressions.size > targetType.length) {
            error("The number of values in the initializer does not match array dimensions.")
        }

        val initializers = init.expressions.flatMap {
            convertStaticInitializer(targetType.element, it)
        }

        val paddingBytes = (targetType.length - init.expressions.size) * (targetType.element.sizeInBytes)
        if (paddingBytes > 0) initializers + ZeroInit(paddingBytes) else initializers
    }

    else -> error("Can't initialize static '${targetType}' with a compound initializer.")
}

private fun typecheckAndConvert(expression: Expression): Expression {
    val checked = typecheck(expression)
    return when (val type = checked.type) {
        // Convert arrays to pointers
        is ArrayType -> AddrOf(checked, PointerType(type.element))
        else -> checked
    }
}

private fun typecheck(currentFunction: FunDeclaration, blockItem: BlockItem): BlockItem = when (blockItem) {
    is FunDeclaration -> typecheck(blockItem)
    is VarDeclaration -> checklocalscope(blockItem)
    is DefaultCase -> DefaultCase(typecheck(currentFunction, blockItem.statement) as Statement, blockItem.label)
    is ExpressionCase -> {
        val expression = typecheckAndConvert(blockItem.expression)

        if (expression.type is DoubleType) {
            error("Case expression constant must be an integer, found 'double'.")
        }

        ExpressionCase(
            expression.cast(switchType.peek()),
            typecheck(currentFunction, blockItem.statement) as Statement, blockItem.label
        )
    }

    is LabeledStatement -> LabeledStatement(
        blockItem.identifier,
        typecheck(currentFunction, blockItem.statement) as Statement
    )

    is Break -> Break(blockItem.identifier)
    is Compound -> Compound(blockItem.block.map { typecheck(currentFunction, it) })
    is Continue -> Continue(blockItem.identifier)
    is DoWhile -> {
        val condition = typecheckAndConvert(blockItem.condition)
        val body = typecheck(currentFunction, blockItem.body) as Statement
        DoWhile(condition, body, blockItem.id)
    }

    is ExpressionStatement -> ExpressionStatement(typecheck(blockItem.expression))
    is For -> {
        val init: ForInit = when (blockItem.init) {
            is InitDecl -> {
                if (blockItem.init.declaration.storageClass != NONE) {
                    error(0, "For initializer cannot have storage specifiers.")
                }

                InitDecl(checklocalscope(blockItem.init.declaration))
            }

            is InitExpr -> InitExpr(blockItem.init.expression?.let { typecheckAndConvert(it) })
        }
        val condition = blockItem.condition?.let { typecheckAndConvert(it) }
        val body = typecheck(currentFunction, blockItem.body) as Statement
        val post = blockItem.post?.let { typecheck(it) }
        For(init, condition, post, body, blockItem.id)
    }

    is Goto -> Goto(blockItem.identifier)
    is If -> {
        val condition = typecheckAndConvert(blockItem.condition)
        val thenBranch = typecheck(currentFunction, blockItem.thenBranch) as Statement
        val elseBranch = blockItem.elseBranch?.let { typecheck(currentFunction, it) } as Statement?
        If(condition, thenBranch, elseBranch)
    }

    NullStatement -> NullStatement
    is ReturnStatement -> {
        val expression = blockItem.value?.let { typecheckAndConvert(it) }
        ReturnStatement(expression?.castForAssignment(currentFunction.type.returnType))
    }

    is Switch -> {
        val expression = typecheckAndConvert(blockItem.expression)

        val type = when (expression.type) {
            // Promote chars to integers.
            is CharacterType -> IntType

            is IntegerType -> expression.type

            else -> if (expression is Var) {
                when (symbolTable[expression.identifier.identifier]?.type) {
                    VoidType -> TODO()
                    is IntegerType -> symbolTable[expression.identifier.identifier]?.type!!
                    is FunType, is PointerType, is DoubleType, is ArrayType, Unknown -> error(
                        expression.identifier.line,
                        "Cannot switch on a '${expression.type}'."
                    )

                    null -> unreachable("Unexpected null type for ${expression.identifier}")
                }
            } else {
                error("Cannot switch on a '${expression.type}'.")
            }
        }

        switchType.push(type)
        val statement = typecheck(currentFunction, blockItem.statement) as Statement
        Switch(expression.cast(type), statement, blockItem.id, blockItem.caseLabels).also {
            switchType.pop()
        }
    }

    is While -> {
        val condition = typecheckAndConvert(blockItem.condition)
        val body = typecheck(currentFunction, blockItem.body) as Statement
        While(condition, body, blockItem.id)
    }
}