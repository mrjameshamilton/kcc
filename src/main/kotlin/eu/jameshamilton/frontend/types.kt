package eu.jameshamilton.frontend

import eu.jameshamilton.unreachable

sealed class Type(val size: Int)
sealed interface Arithmetic
sealed interface IntegerType : Arithmetic

val IntegerType.isSigned: Boolean
    get() = this is IntType || this is LongType
val IntegerType.isUnsigned: Boolean
    get() = !this.isSigned
val Type.isArithmetic: Boolean
    get() = when (this) {
        DoubleType -> true
        is FunType -> false
        IntType -> true
        LongType -> true
        is PointerType -> false
        UIntType -> true
        ULongType -> true
        Unknown -> false
    }

data object Unknown : Type(0)
data object IntType : Type(32), IntegerType {
    override fun toString() = "int"
}

data object LongType : Type(64), IntegerType {
    override fun toString(): String = "long"
}

data object UIntType : Type(32), IntegerType {
    override fun toString(): String = "unsigned int"
}

data object ULongType : Type(64), IntegerType {
    override fun toString(): String = "unsigned long"
}

data object DoubleType : Type(64), Arithmetic {
    override fun toString(): String = "double"
}

data class FunType(val paramsTypes: List<Type>, val returnType: Type) : Type(0) {
    override fun toString(): String =
        "(${paramsTypes.joinToString(separator = ", ") { it.toString() }}) -> $returnType"
}

data class PointerType(val referenced: Type) : Type(0) {
    override fun toString(): String = "$referenced*"
}

val Expression.isNullPointerConstant: Boolean
    get() = when (this) {
        is Constant -> when (value) {
            is Int -> value == 0
            is UInt -> value == 0u
            is Long -> value == 0L
            is ULong -> value == 0uL
            else -> false
        }

        else -> false
    }

infix fun Expression.commonPointerType(b: Expression): Type = when {
    this.type == b.type -> this.type
    this.isNullPointerConstant -> b.type
    b.isNullPointerConstant -> this.type
    else -> unreachable("Invalid pointer cast: either '${this.type}' and '${b.type}' are not valid pointer types.")
}

infix fun Expression.commonType(other: Expression): Type = when {
    this.type is PointerType || other.type is PointerType -> this commonPointerType other
    else -> this.type + other.type
}

operator fun Type.plus(other: Type) = when {
    this == other -> this
    this == DoubleType || other is DoubleType -> DoubleType
    this is Unknown -> this
    this.size == other.size -> if (this is IntegerType && this.isSigned) other else this
    this.size > other.size -> this
    else -> other
}

fun Expression.castForAssignment(targetType: Type): Expression = when {
    type == targetType -> this
    type is Arithmetic && targetType is Arithmetic -> this.cast(targetType)
    isNullPointerConstant && targetType is PointerType -> this.cast(targetType)
    else -> error(0, "Cannot convert type for assignment: '${this.type}' -> '$targetType'.")
}

fun Expression.cast(targetType: Type): Expression = when (this) {
    is Constant -> when (value) {
        is Int -> when (targetType) {
            LongType -> Constant(value.toLong(), targetType)
            IntType -> this
            UIntType -> Constant(value.toUInt(), targetType)
            ULongType -> Constant(value.toULong(), targetType)
            DoubleType -> Constant(value.toDouble(), targetType)
            is PointerType -> {
                if (isNullPointerConstant)
                    Constant(value.toLong(), targetType)
                else
                    unreachable("Invalid cast from '${this.type}' to '$targetType'.")
            }

            is FunType, Unknown -> unreachable("Invalid cast from '${this.type}' to '$targetType'.")
        }

        is Long -> when (targetType) {
            IntType -> Constant(value.toInt(), targetType)
            LongType -> this
            UIntType -> Constant(value.toUInt(), targetType)
            ULongType -> Constant(value.toULong(), targetType)
            DoubleType -> Constant(value.toDouble(), targetType)
            is PointerType -> {
                if (isNullPointerConstant)
                    Constant(value.toLong(), targetType)
                else
                    unreachable("Invalid cast from '${this.type}' to '$targetType'.")
            }

            is FunType, Unknown -> unreachable("Invalid cast from '${this.type}' to '$targetType'.")
        }

        is UInt -> when (targetType) {
            IntType -> Constant(value.toInt(), targetType)
            LongType -> Constant(value.toLong(), targetType)
            UIntType -> this
            ULongType -> Constant(value.toULong(), targetType)
            DoubleType -> Constant(value.toDouble(), targetType)
            is PointerType -> {
                if (isNullPointerConstant)
                    Constant(value.toLong(), targetType)
                else
                    unreachable("Invalid cast from '${this.type}' to '$targetType'.")
            }

            is FunType, Unknown -> unreachable("Invalid cast from '${this.type}' to '$targetType'.")
        }

        is ULong -> when (targetType) {
            IntType -> Constant(value.toInt(), targetType)
            LongType -> Constant(value.toLong(), targetType)
            UIntType -> Constant(value.toUInt(), targetType)
            ULongType -> this
            DoubleType -> Constant(value.toDouble(), targetType)
            is PointerType -> {
                if (isNullPointerConstant)
                    Constant(value.toLong(), targetType)
                else
                    unreachable("Invalid cast from '${this.type}' to '$targetType'.")
            }

            is FunType, Unknown -> unreachable("Invalid cast from '${this.type}' to '$targetType'.")
        }

        is Double -> when (targetType) {
            IntType -> Constant(value.toInt(), targetType)
            LongType -> Constant(value.toLong(), targetType)
            UIntType -> Constant(value.toUInt(), targetType)
            ULongType -> Constant(value.toULong(), targetType)
            DoubleType -> this
            is FunType, Unknown, is PointerType -> unreachable("Invalid cast from '${this.type}' to '$targetType'.")
        }

        else -> Cast(targetType, this, targetType)
    }

    else -> when {
        this.type == targetType -> this

        (this.type is DoubleType && targetType is PointerType) ||
                (targetType is DoubleType && this.type is PointerType) -> {
            error(0, "Cannot cast from '${this.type}' to '${targetType}'.")
        }

        else -> Cast(targetType, this, targetType)
    }
}
