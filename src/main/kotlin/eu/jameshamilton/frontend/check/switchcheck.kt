package eu.jameshamilton.frontend.check

import eu.jameshamilton.frontend.BlockItem
import eu.jameshamilton.frontend.Break
import eu.jameshamilton.frontend.Compound
import eu.jameshamilton.frontend.Constant
import eu.jameshamilton.frontend.Continue
import eu.jameshamilton.frontend.Declaration
import eu.jameshamilton.frontend.DefaultCase
import eu.jameshamilton.frontend.DoWhile
import eu.jameshamilton.frontend.ExpressionCase
import eu.jameshamilton.frontend.ExpressionStatement
import eu.jameshamilton.frontend.For
import eu.jameshamilton.frontend.FunctionDef
import eu.jameshamilton.frontend.Goto
import eu.jameshamilton.frontend.If
import eu.jameshamilton.frontend.LabeledStatement
import eu.jameshamilton.frontend.NullStatement
import eu.jameshamilton.frontend.Program
import eu.jameshamilton.frontend.ReturnStatement
import eu.jameshamilton.frontend.Switch
import eu.jameshamilton.frontend.SwitchCase
import eu.jameshamilton.frontend.While
import eu.jameshamilton.frontend.error

fun checkswitchcases(program: Program) {
    val switches = resolveSwitchCases(program.function)
    switches.forEach { (_, cases) ->
        if (cases.count { it is DefaultCase } > 1) {
            error("Multiple default cases in switch.")
        }

        if (cases.filterIsInstance<ExpressionCase>().groupBy { it.expression }.filter { it.value.size > 1 }
                .isNotEmpty()) {
            error("Duplicate cases in switch.")

            if (cases.filterIsInstance<ExpressionCase>().count { it.expression !is Constant } > 0) {
                error("Case expressions must be constant.")
            }
        }
    }
}

fun resolveSwitchCases(functionDef: FunctionDef): Map<Switch, List<SwitchCase>> {
    val switches = mutableMapOf<Switch, MutableList<SwitchCase>>()

    fun resolve(blockItem: BlockItem, currentSwitch: Switch? = null): Any? = when (blockItem) {
        is ExpressionCase -> {
            switches[currentSwitch!!]!!.add(blockItem)
            resolve(blockItem.statement, currentSwitch)
        }

        is DefaultCase -> {
            switches[currentSwitch!!]!!.add(blockItem)
            resolve(blockItem.statement, currentSwitch)
        }

        is LabeledStatement -> resolve(blockItem.statement, currentSwitch)
        is Compound -> blockItem.block.forEach { resolve(it, currentSwitch) }
        is DoWhile -> resolve(blockItem.body, currentSwitch)
        is ExpressionStatement -> {}
        is For -> resolve(blockItem.body, currentSwitch)
        is If -> {
            resolve(blockItem.thenBranch, currentSwitch)
            blockItem.elseBranch?.let { resolve(it, currentSwitch) }
        }

        is Switch -> {
            switches[blockItem] = mutableListOf()
            resolve(blockItem.statement, blockItem)
        }

        is While -> resolve(blockItem.body, currentSwitch)
        is Declaration, is Break, is Continue, is Goto, NullStatement, is ReturnStatement -> {}
    }

    fun resolve(functionDef: FunctionDef) {
        functionDef.body.forEach { resolve(it) }
    }

    resolve(functionDef)

    return switches
}
