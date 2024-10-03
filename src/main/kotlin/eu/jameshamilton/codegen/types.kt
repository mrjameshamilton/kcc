package eu.jameshamilton.codegen

import eu.jameshamilton.unreachable

sealed interface TypeX86 {
    val size: Bytes
    val suffix: String
}

data object Unknown : TypeX86 {
    override val size: Bytes
        get() = 0L
    override val suffix: String
        get() = unreachable("Unknown suffix $this")
}

data object Byte_ : TypeX86 {
    override val size: Bytes
        get() = 1L
    override val suffix: String
        get() = "b"

}

data object Longword : TypeX86 {
    override val size: Bytes
        get() = 4L
    override val suffix: String
        get() = "l"
}

data object Quadword : TypeX86 {
    override val size: Bytes
        get() = 8L
    override val suffix: String
        get() = "q"
}

data object Double_ : TypeX86 {
    override val size: Bytes
        get() = 8L
    override val suffix: String
        get() = "sd"
}

data class ByteArray(override val size: Long, val alignment: Long) : TypeX86 {

    override val suffix: String
        get() = TODO()
}