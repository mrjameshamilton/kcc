package eu.jameshamilton.frontend.check

import eu.jameshamilton.frontend.BlockItem
import eu.jameshamilton.frontend.Break
import eu.jameshamilton.frontend.Compound
import eu.jameshamilton.frontend.Continue
import eu.jameshamilton.frontend.Declaration
import eu.jameshamilton.frontend.DefaultCase
import eu.jameshamilton.frontend.DoWhile
import eu.jameshamilton.frontend.ExpressionCase
import eu.jameshamilton.frontend.ExpressionStatement
import eu.jameshamilton.frontend.For
import eu.jameshamilton.frontend.FunDeclaration
import eu.jameshamilton.frontend.Goto
import eu.jameshamilton.frontend.Identifier
import eu.jameshamilton.frontend.If
import eu.jameshamilton.frontend.LabeledStatement
import eu.jameshamilton.frontend.NullStatement
import eu.jameshamilton.frontend.Program
import eu.jameshamilton.frontend.ReturnStatement
import eu.jameshamilton.frontend.Switch
import eu.jameshamilton.frontend.While
import eu.jameshamilton.frontend.error

fun checklabels(program: Program) {
    program.functions.forEach { checklabels(it) }
}

fun checklabels(function: FunDeclaration) {
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

    fun check(item: BlockItem?): Any? = when (item) {
        is LabeledStatement -> {
            check(item.identifier)
            check(item.statement)
        }

        is Goto -> {
            gotoLabels.add(item.identifier)
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
        is Switch -> check(item.statement)
        is ExpressionCase -> check(item.statement)
        is DefaultCase -> check(item.statement)
    }

    function.body?.map(::check)

    gotoLabels.forEach {
        if (!labels.contains(it)) {
            error(it.line, "Undefined label '${it.identifier}'.")
        }
    }
}
