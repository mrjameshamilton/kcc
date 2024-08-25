package eu.jameshamilton.frontend.resolve

import eu.jameshamilton.frontend.BlockItem
import eu.jameshamilton.frontend.Break
import eu.jameshamilton.frontend.Compound
import eu.jameshamilton.frontend.Continue
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
import eu.jameshamilton.frontend.Statement
import eu.jameshamilton.frontend.Switch
import eu.jameshamilton.frontend.VarDeclaration
import eu.jameshamilton.frontend.While
import eu.jameshamilton.frontend.error
import java.util.*

fun resolveLabels(program: Program): Program = Program(program.functions.map(::resolveLabels))

private abstract class ResolveIdentifier(val identifier: String) {
    fun toIdentifier(suffix: String = "") = Identifier(identifier + suffix, 0)
}

private class LoopIdentifier(identifier: String) : ResolveIdentifier(identifier)
private class SwitchIdentifier(identifier: String, val caseLabels: MutableList<Identifier>) :
    ResolveIdentifier(identifier)

private val labels = Stack<ResolveIdentifier>()
private var functionCounter = 0
private var idCounter = 0
private val caseLabelCounter = mutableMapOf<SwitchIdentifier, Int>()

private fun <T> scoped(resolveIdentifier: ResolveIdentifier, block: (label: ResolveIdentifier) -> T): T {
    labels.push(resolveIdentifier)
    return block(resolveIdentifier).also { labels.pop() }
}

private fun <T> loop(block: (label: ResolveIdentifier) -> T): T =
    scoped(LoopIdentifier("${"loop"}_${functionCounter}.${idCounter++}"), block)

private fun <T> switch(block: (label: ResolveIdentifier) -> T): T =
    scoped(SwitchIdentifier("${"switch"}_${functionCounter}.${idCounter++}", mutableListOf()), block)

// Create a label unique for the whole program, since the same
// label could appear in multiple functions.
private fun unique(identifier: Identifier): Identifier {
    return Identifier(identifier.identifier + "_" + functionCounter, identifier.line)
}

private fun resolveLabels(funDeclaration: FunDeclaration): FunDeclaration {
    functionCounter++
    idCounter = 0

    fun resolve(blockItem: BlockItem): BlockItem = when (blockItem) {
        is DoWhile -> loop { label ->
            DoWhile(blockItem.condition, resolve(blockItem.body) as Statement, label.toIdentifier())
        }

        is While -> loop { label ->
            While(blockItem.condition, resolve(blockItem.body) as Statement, label.toIdentifier())
        }

        is For -> loop { label ->
            For(
                blockItem.init,
                blockItem.condition,
                blockItem.post,
                resolve(blockItem.body) as Statement,
                label.toIdentifier()
            )
        }

        is Break -> {
            if (labels.isEmpty()) {
                error("'break' outside of loop or switch.")
            } else {
                val identifier = labels.peek()
                Break(identifier.toIdentifier("_break"))
            }
        }

        is Continue -> {
            val identifier = labels.lastOrNull { it is LoopIdentifier }
            if (identifier == null) {
                error("'continue' outside of loop.")
            } else {
                Continue(identifier.toIdentifier("_continue"))
            }
        }

        is LabeledStatement -> LabeledStatement(unique(blockItem.identifier), resolve(blockItem.statement) as Statement)
        is Goto -> Goto(unique(blockItem.identifier))
        is VarDeclaration -> VarDeclaration(blockItem.identifier, blockItem.initializer)
        is FunDeclaration -> FunDeclaration(blockItem.identifier, blockItem.params, blockItem.body?.map { resolve(it) })
        is Compound -> Compound(blockItem.block.map { resolve(it) })
        is ExpressionStatement -> ExpressionStatement(blockItem.expression)
        is If -> If(blockItem.condition, resolve(blockItem.thenBranch) as Statement,
            blockItem.elseBranch?.let { resolve(it) } as Statement?)

        is ReturnStatement -> ReturnStatement(blockItem.value)
        NullStatement -> NullStatement
        is Switch -> switch { label ->
            require(label is SwitchIdentifier)
            Switch(
                blockItem.expression,
                resolve(blockItem.statement) as Statement,
                label.toIdentifier(),
                label.caseLabels
            )
        }

        is ExpressionCase -> {
            val identifier = labels.lastOrNull { it is SwitchIdentifier } as SwitchIdentifier?
            if (identifier == null) {
                error("'case' outside of 'switch'")
            } else {

                val caseLabel =
                    Identifier(identifier.identifier + "_case_" + caseLabelCounter.merge(identifier, 1, Int::plus), 0)
                identifier.caseLabels.add(caseLabel)
                ExpressionCase(
                    blockItem.expression, resolve(blockItem.statement) as Statement,
                    caseLabel
                )
            }
        }

        is DefaultCase -> {
            val switchIdentifier = labels.lastOrNull { it is SwitchIdentifier } as SwitchIdentifier?
            if (switchIdentifier == null) {
                error("'default' outside of 'switch'")
            } else {
                val defaultLabel = Identifier(switchIdentifier.identifier + "_default", 0)
                switchIdentifier.caseLabels.add(defaultLabel)
                DefaultCase(resolve(blockItem.statement) as Statement, defaultLabel)
            }
        }
    }

    return FunDeclaration(funDeclaration.identifier, funDeclaration.params, funDeclaration.body?.map(::resolve))
}