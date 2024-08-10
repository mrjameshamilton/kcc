package eu.jameshamilton

fun unreachable(reason: String): Nothing {
    throw RuntimeException("unreachable: $reason")
}
