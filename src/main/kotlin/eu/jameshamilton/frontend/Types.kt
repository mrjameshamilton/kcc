package eu.jameshamilton.frontend

sealed class Type
data object IntType : Type() {
    override fun toString() = "int"
}

data object LongType : Type() {
    override fun toString(): String = "long"
}

data class FunType(val paramsTypes: List<Type>, val returnType: Type) : Type()
