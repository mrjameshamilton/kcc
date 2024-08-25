package eu.jameshamilton.frontend.resolve

import eu.jameshamilton.frontend.Assignment
import eu.jameshamilton.frontend.BinaryExpr
import eu.jameshamilton.frontend.BlockItem
import eu.jameshamilton.frontend.Break
import eu.jameshamilton.frontend.Compound
import eu.jameshamilton.frontend.Conditional
import eu.jameshamilton.frontend.Constant
import eu.jameshamilton.frontend.Continue
import eu.jameshamilton.frontend.Declaration
import eu.jameshamilton.frontend.DefaultCase
import eu.jameshamilton.frontend.DoWhile
import eu.jameshamilton.frontend.Expression
import eu.jameshamilton.frontend.ExpressionCase
import eu.jameshamilton.frontend.ExpressionStatement
import eu.jameshamilton.frontend.For
import eu.jameshamilton.frontend.FunDeclaration
import eu.jameshamilton.frontend.FunctionCall
import eu.jameshamilton.frontend.Goto
import eu.jameshamilton.frontend.Identifier
import eu.jameshamilton.frontend.If
import eu.jameshamilton.frontend.InitDecl
import eu.jameshamilton.frontend.InitExpr
import eu.jameshamilton.frontend.LabeledStatement
import eu.jameshamilton.frontend.NullStatement
import eu.jameshamilton.frontend.Program
import eu.jameshamilton.frontend.ReturnStatement
import eu.jameshamilton.frontend.Statement
import eu.jameshamilton.frontend.Switch
import eu.jameshamilton.frontend.UnaryExpr
import eu.jameshamilton.frontend.UnaryOp.PostfixDecrement
import eu.jameshamilton.frontend.UnaryOp.PostfixIncrement
import eu.jameshamilton.frontend.UnaryOp.PrefixDecrement
import eu.jameshamilton.frontend.UnaryOp.PrefixIncrement
import eu.jameshamilton.frontend.Var
import eu.jameshamilton.frontend.VarDeclaration
import eu.jameshamilton.frontend.While
import eu.jameshamilton.frontend.error
import eu.jameshamilton.frontend.resolve.Linkage.EXTERNAL
import eu.jameshamilton.frontend.resolve.Linkage.NONE
import java.util.*

fun resolveVariables(program: Program): Program = scoped {
    Program(program.declarations.map { resolve(it) })
}

private enum class Linkage {
    NONE, EXTERNAL
}

private data class Variable(val identifier: Identifier, val linkage: Linkage = NONE, val level: Int = 0)

private val scopes = Stack<MutableMap<String, Variable>>()
private val variables: MutableMap<String, Variable>
    get() = scopes.peek()

private fun <T> scoped(block: () -> T): T {
    scopes.push(HashMap(if (scopes.isNotEmpty()) scopes.peek() else mutableMapOf()))
    return block().also {
        // scopes.forEach { println(it) }
        scopes.pop()
    }
}

private fun maketemporary(name: Identifier, linkage: Linkage): Variable {
    val newName = if (linkage == EXTERNAL) name.identifier else "${name.identifier}.${scopes.size}.${variables.size}"
    return Variable(Identifier(newName, name.line), linkage, scopes.size)
}

private fun resolve(name: Identifier, linkage: Linkage): Variable {
    val alreadyDeclaredInScope =
        variables.containsKey(name.identifier) &&
                variables[name.identifier]?.level == scopes.size

    if (alreadyDeclaredInScope) {
        val variable = variables[name.identifier]!!

        // External linkage declarations can be declared multiple times.
        if (variable.linkage == EXTERNAL && linkage == EXTERNAL) {
            return variable
        }

        if (linkage != variable.linkage) {
            error(name.line, "Duplicate declaration '${name.identifier}' with different linkage.")
        } else {
            error(name.line, "Duplicate declaration '${name.identifier}'.")
        }
    }

    val uniqueName = maketemporary(name, linkage)
    variables[name.identifier] = uniqueName
    return uniqueName
}

private fun resolve(expression: Expression): Expression = when (expression) {
    is Assignment -> {
        val left = expression.lvalue
        val right = expression.value
        if (left !is Var) {
            error(0, "Expression is not an lvalue.")
        }
        Assignment(resolve(left), resolve(right))
    }

    is Var -> if (expression.identifier.identifier in variables.keys) {
        Var(variables[expression.identifier.identifier]!!.identifier)
    } else {
        error(
            expression.identifier.line,
            "Undeclared variable '${expression.identifier.identifier}'."
        )
    }

    is BinaryExpr -> BinaryExpr(resolve(expression.left), expression.operator, resolve(expression.right))
    is Constant -> Constant(expression.value)
    is UnaryExpr -> {
        when (expression.op) {
            PrefixIncrement, PostfixIncrement, PrefixDecrement, PostfixDecrement ->
                if (expression.expression !is Var) {
                    error(0, "Expression is not assignable.")
                }

            else -> {}
        }
        UnaryExpr(expression.op, resolve(expression.expression))
    }

    is Conditional -> Conditional(
        resolve(expression.condition),
        resolve(expression.thenBranch),
        resolve(expression.elseBranch)
    )

    is FunctionCall -> {
        val name = variables[expression.identifier.identifier]
        if (name == null) {
            error(expression.identifier.line, "Function '${expression.identifier.identifier}' not defined.")
        } else {
            FunctionCall(name.identifier, expression.arguments.map { resolve(it) })
        }
    }
}

private fun resolve(funDeclaration: FunDeclaration): FunDeclaration {
    val name = resolve(funDeclaration.identifier, EXTERNAL)
    return scoped {
        val arguments = funDeclaration.params?.map {
            resolve(it, NONE).identifier
        }
        val body = funDeclaration.body?.map { resolve(it) }
        FunDeclaration(name.identifier, arguments, body)
    }
}

private fun resolve(varDeclaration: VarDeclaration): VarDeclaration {
    //TODO
    val name = varDeclaration.identifier.identifier
    return varDeclaration
}

private fun resolve(declaration: Declaration) = when (declaration) {
    is FunDeclaration -> resolve(declaration)
    is VarDeclaration -> resolve(declaration)
}

private fun resolve(blockItem: BlockItem): BlockItem = when (blockItem) {
    is Declaration -> {
        val name = resolve(blockItem.identifier, if (blockItem is FunDeclaration) EXTERNAL else NONE)

        when (blockItem) {
            is VarDeclaration -> {
                if (blockItem.initializer == null) {
                    VarDeclaration(name.identifier, null)
                } else {
                    VarDeclaration(name.identifier, resolve(blockItem.initializer))
                }
            }

            is FunDeclaration -> {
                if (blockItem.body != null) {
                    error(
                        blockItem.identifier.line,
                        "Nested function '${blockItem.identifier.identifier}' definition not allowed."
                    )
                }

                resolve(blockItem)
            }
        }
    }

    is ExpressionStatement -> ExpressionStatement(resolve(blockItem.expression))
    is ReturnStatement -> ReturnStatement(resolve(blockItem.value))
    is If -> If(
        resolve(blockItem.condition),
        resolve(blockItem.thenBranch) as Statement,
        blockItem.elseBranch?.let { resolve(it) } as Statement?)

    is Compound -> scoped {
        Compound(blockItem.block.map { resolve(it) })
    }

    NullStatement -> NullStatement
    is Goto -> Goto(blockItem.identifier)
    is LabeledStatement -> LabeledStatement(blockItem.identifier, resolve(blockItem.statement) as Statement)
    is Break -> Break(blockItem.identifier)
    is Continue -> Continue(blockItem.identifier)
    is DoWhile -> DoWhile(resolve(blockItem.condition), resolve(blockItem.body) as Statement)
    is While -> While(resolve(blockItem.condition), resolve(blockItem.body) as Statement)
    is For -> scoped {
        val init = when (blockItem.init) {
            is InitDecl -> InitDecl(resolve(blockItem.init.declaration) as VarDeclaration)
            is InitExpr -> InitExpr(blockItem.init.expression?.let { resolve(it) })
        }
        val condition = blockItem.condition?.let { resolve(it) }
        val post = blockItem.post?.let { resolve(it) }
        val body = resolve(blockItem.body) as Statement

        For(init, condition, post, body)
    }

    is Switch -> Switch(resolve(blockItem.expression), resolve(blockItem.statement) as Statement)
    is ExpressionCase -> ExpressionCase(resolve(blockItem.expression), resolve(blockItem.statement) as Statement)
    is DefaultCase -> DefaultCase(resolve(blockItem.statement) as Statement)
}
