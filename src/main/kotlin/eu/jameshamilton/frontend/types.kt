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

data class FunType(val paramsTypes: List<Type>, val returnType: Type) : Type(0) {
    override fun toString(): String =
        "(${paramsTypes.joinToString(separator = ", ") { it.toString() }}) -> $returnType"
}

val Type.isSigned: Boolean
    get() = this is UIntType || this is ULongType

operator fun Type.plus(other: Type) = when {
    this == other -> this
    this is Unknown -> this
    this.size == other.size -> if (this.isSigned) other else this
    this.size > other.size -> this
    else -> other
}

fun Expression.cast(type: Type): Expression = when (this) {
    is Constant -> when (value) {
        is Int -> when (type) {
            LongType -> Constant(value.toLong(), type)
            IntType -> this
            UIntType -> Constant(value.toUInt(), type)
            ULongType -> Constant(value.toULong(), type)
            is FunType, Unknown -> unreachable("Invalid cast")
        }

        is Long -> when (type) {
            IntType -> Constant(value.toInt(), type)
            LongType -> this
            UIntType -> Constant(value.toUInt(), type)
            ULongType -> Constant(value.toULong(), type)
            is FunType, Unknown -> unreachable("Invalid cast")
        }

        is UInt -> when (type) {
            IntType -> Constant(value.toInt(), type)
            LongType -> Constant(value.toLong(), type)
            UIntType -> this
            ULongType -> Constant(value.toULong(), type)
            is FunType, Unknown -> unreachable("Invalid cast")
        }

        is ULong -> when (type) {
            IntType -> Constant(value.toInt(), type)
            LongType -> Constant(value.toLong(), type)
            UIntType -> Constant(value.toUInt(), type)
            ULongType -> this
            is FunType, Unknown -> unreachable("Invalid cast")
        }

        else -> Cast(type, this, type)
    }

    else -> when {
        this.type == type -> this
        else -> Cast(type, this, type)
    }
}
