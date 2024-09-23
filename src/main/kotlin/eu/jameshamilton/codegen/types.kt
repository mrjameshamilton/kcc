package eu.jameshamilton.codegen

import eu.jameshamilton.unreachable

sealed interface TypeX86 {
    val size: Bytes
    val suffix: String
}

data object Unknown : TypeX86 {
    override val size: Bytes
        get() = 0
    override val suffix: String
        get() = unreachable("Unknown suffix $this")
}

data object Byte_ : TypeX86 {
    override val size: Bytes
        get() = 1
    override val suffix: String
        get() = "b"

}

data object Longword : TypeX86 {
    override val size: Bytes
        get() = 4
    override val suffix: String
        get() = "l"
}

data object Quadword : TypeX86 {
    override val size: Bytes
        get() = 8
    override val suffix: String
        get() = "q"
}

data object Double_ : TypeX86 {
    override val size: Bytes
        get() = 8
    override val suffix: String
        get() = "sd"
}

data class ByteArray(override val size: Int, val alignment: Int) : TypeX86 {

    override val suffix: String
        get() = TODO()
}