package eu.jameshamilton.frontend

fun checklabels(program: Program) {
    val labels = mutableSetOf<Identifier>()

    fun check(identifier: Identifier) {
        if (!labels.add(identifier)) {
            error(
                identifier.line,
                "Label '${identifier.identifier}' already defined on line ${labels.find { it == identifier }?.line}."
            )
        }
    }

    fun check(item: BlockItem?): Unit = when (item) {
        is Label -> check(item.identifier)
        is LabeledStatement -> {
            check(item.identifier)
            check(item.statement)
        }

        is If -> {
            check(item.thenBranch)
            check(item.elseBranch)
        }

        is Compound -> item.block.forEach(::check)

        is Declaration, is ExpressionStatement, is Goto, NullStatement, is ReturnStatement, null -> {}
    }

    program.function.body.map(::check)
}
