package eu.jameshamilton.frontend

sealed class Type
data object IntType : Type()
data object LongType : Type()
data class FunType(val paramsTypes: List<Type>, val returnType: Type) : Type()
