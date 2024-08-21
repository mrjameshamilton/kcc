package eu.jameshamilton.frontend.check

import eu.jameshamilton.frontend.BlockItem
import eu.jameshamilton.frontend.Break
import eu.jameshamilton.frontend.Case
import eu.jameshamilton.frontend.Compound
import eu.jameshamilton.frontend.Continue
import eu.jameshamilton.frontend.Declaration
import eu.jameshamilton.frontend.Default
import eu.jameshamilton.frontend.DoWhile
import eu.jameshamilton.frontend.ExpressionStatement
import eu.jameshamilton.frontend.For
import eu.jameshamilton.frontend.Goto
import eu.jameshamilton.frontend.Identifier
import eu.jameshamilton.frontend.If
import eu.jameshamilton.frontend.LabeledStatement
import eu.jameshamilton.frontend.NullStatement
import eu.jameshamilton.frontend.Program
import eu.jameshamilton.frontend.ReturnStatement
import eu.jameshamilton.frontend.Switch
import eu.jameshamilton.frontend.While

fun checklabels(program: Program) {
    val gotoLabels = mutableSetOf<Identifier>()
    val labels = mutableSetOf<Identifier>()

    fun check(identifier: Identifier) {
        if (!labels.add(identifier)) {
            eu.jameshamilton.frontend.error(
                identifier.line,
                "Label '${identifier.identifier}' already defined on line ${labels.find { it == identifier }?.line}."
            )
        }
    }

    fun check(item: BlockItem?): Unit = when (item) {
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
        is Switch -> {}
        is Case -> {}
        is Default -> {}
    }

    program.function.body.map(::check)

    gotoLabels.forEach {
        if (!labels.contains(it)) {
            eu.jameshamilton.frontend.error(it.line, "Undefined label '${it.identifier}'.")
        }
    }
}
