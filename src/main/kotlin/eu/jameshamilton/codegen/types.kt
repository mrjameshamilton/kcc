package eu.jameshamilton.codegen

sealed interface TypeX86

data object Unknown : TypeX86
data object Longword : TypeX86
data object Quadword : TypeX86
