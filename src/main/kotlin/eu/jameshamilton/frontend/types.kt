package eu.jameshamilton.frontend

import eu.jameshamilton.unreachable

sealed class Type(val size: Int)

data object Unknown : Type(0)
data object IntType : Type(32) {
    override fun toString() = "int"
}

data object LongType : Type(64) {
    override fun toString(): String = "long"
}

data object UIntType : Type(32) {
    override fun toString(): String = "unsigned int"
}

data object ULongType : Type(64) {
    override fun toString(): String = "unsigned long"
}

data object DoubleType : Type(64) {
    override fun toString(): String = "double"
}

data class FunType(val paramsTypes: List<Type>, val returnType: Type) : Type(0) {
    override fun toString(): String =
        "(${paramsTypes.joinToString(separator = ", ") { it.toString() }}) -> $returnType"
}

val Type.isSigned: Boolean
    get() = this is IntType || this is LongType

operator fun Type.plus(other: Type) = when {
    this == other -> this
    this == DoubleType || other is DoubleType -> DoubleType
    this is Unknown -> this
    this.size == other.size -> if (this.isSigned) other else this
    this.size > other.size -> this
    else -> other
}

fun Expression.cast(targetType: Type): Expression = when (this) {
    is Constant -> when (value) {
        is Int -> when (targetType) {
            LongType -> Constant(value.toLong(), targetType)
            IntType -> this
            UIntType -> Constant(value.toUInt(), targetType)
            ULongType -> Constant(value.toULong(), targetType)
            DoubleType -> Constant(value.toDouble(), targetType)
            is FunType, Unknown -> unreachable("Invalid cast")
        }

        is Long -> when (targetType) {
            IntType -> Constant(value.toInt(), targetType)
            LongType -> this
            UIntType -> Constant(value.toUInt(), targetType)
            ULongType -> Constant(value.toULong(), targetType)
            DoubleType -> Constant(value.toDouble(), targetType)
            is FunType, Unknown -> unreachable("Invalid cast")
        }

        is UInt -> when (targetType) {
            IntType -> Constant(value.toInt(), targetType)
            LongType -> Constant(value.toLong(), targetType)
            UIntType -> this
            ULongType -> Constant(value.toULong(), targetType)
            DoubleType -> Constant(value.toDouble(), targetType)
            is FunType, Unknown -> unreachable("Invalid cast")
        }

        is ULong -> when (targetType) {
            IntType -> Constant(value.toInt(), targetType)
            LongType -> Constant(value.toLong(), targetType)
            UIntType -> Constant(value.toUInt(), targetType)
            ULongType -> this
            DoubleType -> Constant(value.toDouble(), targetType)
            is FunType, Unknown -> unreachable("Invalid cast")
        }

        is Double -> when (targetType) {
            IntType -> Constant(value.toInt(), targetType)
            LongType -> Constant(value.toLong(), targetType)
            UIntType -> Constant(value.toUInt(), targetType)
            ULongType -> Constant(value.toULong(), targetType)
            DoubleType -> this
            is FunType, Unknown -> unreachable("Invalid cast")
        }

        else -> Cast(targetType, this, targetType)
    }

    else -> when {
        this.type == targetType -> this
        else -> Cast(targetType, this, targetType)
    }
}
