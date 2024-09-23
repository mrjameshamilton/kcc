package eu.jameshamilton.frontend

import eu.jameshamilton.unreachable

sealed class Type(open val sizeInBits: Int) {
    val sizeInBytes: Int
        get() = sizeInBits / 8

    val baseType: Type by lazy {
        fun baseType(type: Type): Type = when (type) {
            is ArrayType -> baseType(type.element)
            is PointerType -> baseType(type.referenced)
            else -> type
        }

        baseType(this)
    }
}

sealed interface ArithmeticType
sealed interface IntegerType : ArithmeticType
sealed interface CharacterType : IntegerType

val IntegerType.isSigned: Boolean
    get() = this is IntType || this is LongType || this is CharType || this is SCharType
val IntegerType.isUnsigned: Boolean
    get() = !this.isSigned

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

data object DoubleType : Type(64), ArithmeticType {
    override fun toString(): String = "double"
}

data object CharType : Type(8), CharacterType {
    override fun toString(): String = "char"
}

data object SCharType : Type(8), CharacterType {
    override fun toString(): String = "signed char"
}

data object UCharType : Type(8), CharacterType {
    override fun toString(): String = "unsigned char"
}

data class ArrayType(val element: Type, val length: Int) : Type(0) {

    override val sizeInBits: Int
        get() = length * element.sizeInBits

    override fun toString(): String {
        return "$element[$length]"
    }
}

data class FunType(val paramsTypes: List<Type>, val returnType: Type) : Type(0) {
    override fun toString(): String =
        "(${paramsTypes.joinToString(separator = ", ") { it.toString() }}) -> $returnType"
}

data class PointerType(val referenced: Type) : Type(64) {
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
    this is CharacterType || other is CharacterType -> IntType
    this is Unknown -> this
    this.sizeInBits == other.sizeInBits -> if (this is IntegerType && this.isSigned) other else this
    this.sizeInBits > other.sizeInBits -> this
    else -> other
}

fun Expression.castForAssignment(targetType: Type): Expression = when {
    type == targetType -> this
    type is ArithmeticType && targetType is ArithmeticType -> this.cast(targetType)
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
            CharType -> Constant(value.toInt(), targetType)
            SCharType -> Constant(value.toInt(), targetType)
            UCharType -> Constant(value.toUInt(), targetType)
            is PointerType -> {
                if (isNullPointerConstant)
                    Constant(0UL, targetType)
                else
                    unreachable("Invalid cast from '${this.type}' to '$targetType'.")
            }

            is FunType, Unknown -> unreachable("Invalid cast from '${this.type}' to '$targetType'.")
            is ArrayType -> TODO()
        }

        is Long -> when (targetType) {
            IntType -> Constant(value.toInt(), targetType)
            LongType -> this
            UIntType -> Constant(value.toUInt(), targetType)
            ULongType -> Constant(value.toULong(), targetType)
            DoubleType -> Constant(value.toDouble(), targetType)
            CharType -> Constant(value.toInt(), targetType)
            SCharType -> Constant(value.toInt(), targetType)
            UCharType -> Constant(value.toUInt(), targetType)
            is PointerType -> {
                if (isNullPointerConstant)
                    Constant(0UL, targetType)
                else
                    unreachable("Invalid cast from '${this.type}' to '$targetType'.")
            }

            is FunType, Unknown -> unreachable("Invalid cast from '${this.type}' to '$targetType'.")
            is ArrayType -> TODO()
        }

        is UInt -> when (targetType) {
            IntType -> Constant(value.toInt(), targetType)
            LongType -> Constant(value.toLong(), targetType)
            UIntType -> this
            ULongType -> Constant(value.toULong(), targetType)
            DoubleType -> Constant(value.toDouble(), targetType)
            CharType -> Constant(value.toInt(), targetType)
            SCharType -> Constant(value.toInt(), targetType)
            UCharType -> Constant(value.toUInt(), targetType)
            is PointerType -> {
                if (isNullPointerConstant)
                    Constant(0UL, targetType)
                else
                    unreachable("Invalid cast from '${this.type}' to '$targetType'.")
            }

            is FunType, Unknown -> unreachable("Invalid cast from '${this.type}' to '$targetType'.")
            is ArrayType -> TODO()
        }

        is ULong -> when (targetType) {
            IntType -> Constant(value.toInt(), targetType)
            LongType -> Constant(value.toLong(), targetType)
            UIntType -> Constant(value.toUInt(), targetType)
            ULongType -> this
            DoubleType -> Constant(value.toDouble(), targetType)
            CharType -> Constant(value.toInt(), targetType)
            SCharType -> Constant(value.toInt(), targetType)
            UCharType -> Constant(value.toUInt(), targetType)
            is PointerType -> {
                if (isNullPointerConstant)
                    Constant(0UL, targetType)
                else
                    unreachable("Invalid cast from '${this.type}' to '$targetType'.")
            }

            is FunType, Unknown -> unreachable("Invalid cast from '${this.type}' to '$targetType'.")
            is ArrayType -> TODO()
        }

        is Double -> when (targetType) {
            IntType -> Constant(value.toInt(), targetType)
            LongType -> Constant(value.toLong(), targetType)
            UIntType -> Constant(value.toUInt(), targetType)
            ULongType -> Constant(value.toULong(), targetType)
            DoubleType -> this
            CharType -> Constant(value.toInt(), targetType)
            SCharType -> Constant(value.toInt(), targetType)
            UCharType -> Constant(value.toUInt(), targetType)
            is FunType, Unknown, is PointerType -> unreachable("Invalid cast from '${this.type}' to '$targetType'.")
            is ArrayType -> TODO()
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

fun Type.pointerTo(): PointerType = PointerType(this)
