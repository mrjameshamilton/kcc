package eu.jameshamilton.codegen

import eu.jameshamilton.unreachable

sealed interface TypeX86 {
    val size: Bytes
    val suffix: Char
}

data object Unknown : TypeX86 {
    override val size: Bytes
        get() = 0
    override val suffix: Char
        get() = unreachable("Unknown suffix $this")
}

data object Longword : TypeX86 {
    override val size: Bytes
        get() = 4
    override val suffix: Char
        get() = 'l'
}

data object Quadword : TypeX86 {
    override val size: Bytes
        get() = 8
    override val suffix: Char
        get() = 'q'
}
