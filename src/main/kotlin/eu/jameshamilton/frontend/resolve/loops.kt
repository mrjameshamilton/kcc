package eu.jameshamilton.frontend.resolve

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
import eu.jameshamilton.frontend.FunctionDef
import eu.jameshamilton.frontend.Goto
import eu.jameshamilton.frontend.Identifier
import eu.jameshamilton.frontend.If
import eu.jameshamilton.frontend.LabeledStatement
import eu.jameshamilton.frontend.NullStatement
import eu.jameshamilton.frontend.Program
import eu.jameshamilton.frontend.ReturnStatement
import eu.jameshamilton.frontend.Statement
import eu.jameshamilton.frontend.Switch
import eu.jameshamilton.frontend.While
import eu.jameshamilton.frontend.error
import java.util.*

fun resolveLoopLabels(program: Program): Program {
    return Program(resolveLoopLabels(program.function))
}

private fun resolveLoopLabels(functionDef: FunctionDef): FunctionDef {
    abstract class ResolveIdentifier(val identifier: String) {
        fun toIdentifier() = Identifier(identifier, 0)
    }

    class LoopIdentifier(identifier: String) : ResolveIdentifier(identifier)
    class SwitchIdentifier(identifier: String) : ResolveIdentifier(identifier)

    val labels = Stack<ResolveIdentifier>()
    var counter = 0

    fun <T> scoped(resolveIdentifier: ResolveIdentifier, block: (label: ResolveIdentifier) -> T): T {
        labels.push(resolveIdentifier)
        return block(resolveIdentifier).also { labels.pop() }
    }

    fun <T> loop(block: (label: ResolveIdentifier) -> T): T =
        scoped(LoopIdentifier("${"loop"}_${counter++}"), block)

    fun <T> switch(block: (label: ResolveIdentifier) -> T): T =
        scoped(SwitchIdentifier("${"switch"}_${counter++}"), block)

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
                Break(identifier.toIdentifier())
            }
        }

        is Continue -> {
            val identifier = labels.lastOrNull { it is LoopIdentifier }
            if (identifier == null) {
                error("'continue' outside of loop.")
            } else {
                Continue(identifier.toIdentifier())
            }
        }

        is LabeledStatement -> LabeledStatement(blockItem.identifier, resolve(blockItem.statement) as Statement)
        is Declaration -> Declaration(blockItem.identifier, blockItem.initializer)
        is Compound -> Compound(blockItem.block.map { resolve(it) })
        is ExpressionStatement -> ExpressionStatement(blockItem.expression)
        is Goto -> Goto(blockItem.identifier)
        is If -> If(blockItem.condition, resolve(blockItem.thenBranch) as Statement,
            blockItem.elseBranch?.let { resolve(it) } as Statement?)

        is ReturnStatement -> ReturnStatement(blockItem.value)
        NullStatement -> NullStatement
        is Switch -> switch { label ->
            Switch(blockItem.expression, resolve(blockItem.statement) as Statement, label.toIdentifier())
        }

        is Case -> {
            if (labels.count { it is SwitchIdentifier } == 0) {
                error("'case' outside of 'switch'")
            } else {
                val identifier = labels.last { it is SwitchIdentifier }
                Case(blockItem.expression, resolve(blockItem.statement) as Statement, identifier.toIdentifier())
            }
        }

        is Default -> {
            val identifier = labels.lastOrNull { it is SwitchIdentifier }
            if (identifier == null) {
                error("'default' outside of 'switch'")
            } else {
                Default(resolve(blockItem.statement) as Statement, identifier.toIdentifier())
            }
        }
    }

    return FunctionDef(functionDef.name, functionDef.body.map(::resolve))
}