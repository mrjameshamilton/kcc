package eu.jameshamilton.frontend

fun checklabels(program: Program) {
    val gotoLabels = mutableSetOf<Identifier>()
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

        is Goto -> {
            gotoLabels.add(item.identifier)
            Unit // TODO: refactor?
        }

        is If -> {
            check(item.thenBranch)
            check(item.elseBranch)
        }

        is Compound -> item.block.forEach(::check)

        is Declaration, is ExpressionStatement, NullStatement, is ReturnStatement, is Break, is Continue, null -> {}
        is DoWhile -> check(item.body)
        is While -> check(item.body)
        is For -> check(item.body)
    }

    program.function.body.map(::check)

    gotoLabels.forEach {
        if (!labels.contains(it)) {
            error(it.line, "Undefined label '${it.identifier}'.")
        }
    }
}
