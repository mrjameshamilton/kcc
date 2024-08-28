package eu.jameshamilton.frontend

sealed class Type
data object IntType : Type() {
    override fun toString() = "int"
}

data object LongType : Type() {
    override fun toString(): String = "long"
}

data class FunType(val paramsTypes: List<Type>, val returnType: Type) : Type() {
    override fun toString(): String =
        "(${paramsTypes.joinToString(separator = ", ") { it.toString() }}) -> $returnType"
}

operator fun Type.plus(other: Type) = when {
    this == other -> this
    else -> LongType
}

fun Expression.cast(type: Type): Expression = when (this) {
    is Constant -> when (value) {
        is Int -> when (type) {
            LongType -> Constant(value.toLong(), type)
            else -> Constant(value, type)
        }

        is Long -> when (type) {
            IntType -> Constant(value.toInt(), type)
            else -> Constant(value, type)
        }

        else -> Cast(type, this, type)
    }

    else -> when {
        this.type == type -> this
        else -> Cast(type, this, type)
    }
}
