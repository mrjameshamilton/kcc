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

fun resolveLabels(program: Program): Program {
    return Program(program.functions.map { resolveLabels(it) })
}

private fun resolveLabels(funDeclaration: FunDeclaration): FunDeclaration {
    abstract class ResolveIdentifier(val identifier: String) {
        fun toIdentifier(suffix: String = "") = Identifier(identifier + suffix, 0)
    }

    class LoopIdentifier(identifier: String) : ResolveIdentifier(identifier)
    class SwitchIdentifier(identifier: String, val caseLabels: MutableList<Identifier>) : ResolveIdentifier(identifier)

    val labels = Stack<ResolveIdentifier>()
    var idCounter = 0
    val caseLabelCounter = mutableMapOf<SwitchIdentifier, Int>()

    fun <T> scoped(resolveIdentifier: ResolveIdentifier, block: (label: ResolveIdentifier) -> T): T {
        labels.push(resolveIdentifier)
        return block(resolveIdentifier).also { labels.pop() }
    }

    fun <T> loop(block: (label: ResolveIdentifier) -> T): T =
        scoped(LoopIdentifier("${"loop"}_${idCounter++}"), block)

    fun <T> switch(block: (label: ResolveIdentifier) -> T): T =
        scoped(SwitchIdentifier("${"switch"}_${idCounter++}", mutableListOf()), block)

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

        is LabeledStatement -> LabeledStatement(blockItem.identifier, resolve(blockItem.statement) as Statement)
        is VarDeclaration -> VarDeclaration(blockItem.identifier, blockItem.initializer)
        is FunDeclaration -> TODO()
        is Compound -> Compound(blockItem.block.map { resolve(it) })
        is ExpressionStatement -> ExpressionStatement(blockItem.expression)
        is Goto -> Goto(blockItem.identifier)
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