package eu.jameshamilton

fun unreachable(reason: String): Nothing {
    throw RuntimeException("unreachable: $reason")
}

val Double.isNegativeZero
    get() = this.toBits().toULong() > 0u
