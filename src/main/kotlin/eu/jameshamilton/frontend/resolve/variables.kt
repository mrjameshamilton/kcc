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
import eu.jameshamilton.frontend.FunctionDef
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
import eu.jameshamilton.frontend.While
import java.util.*

fun resolveVariables(program: Program): Program {
    return Program(resolveVariables(program.function))
}

fun resolveVariables(functionDef: FunctionDef): FunctionDef {
    data class Variable(val identifier: Identifier, val level: Int = 0)

    val scopes = Stack<MutableMap<String, Variable>>()
    fun variables(): MutableMap<String, Variable> = scopes.peek()

    fun <T> scoped(block: () -> T): T {
        scopes.push(HashMap(if (scopes.isNotEmpty()) scopes.peek() else mutableMapOf()))
        return block().also { scopes.pop() }
    }

    fun maketemporary(name: Identifier): Variable =
        Variable(Identifier("${name.identifier}.${scopes.size}.${variables().size}", name.line), scopes.size)

    fun resolve(expression: Expression): Expression = when (expression) {
        is Assignment -> {
            val left = expression.lvalue
            val right = expression.value
            if (left !is Var) {
                eu.jameshamilton.frontend.error(0, "Expression is not an lvalue.")
            }
            Assignment(resolve(left), resolve(right))
        }

        is Var -> if (expression.identifier.identifier in variables().keys) {
            Var(variables()[expression.identifier.identifier]!!.identifier)
        } else {
            eu.jameshamilton.frontend.error(
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
                        eu.jameshamilton.frontend.error(0, "Expression is not assignable.")
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
    }

    fun resolve(blockItem: BlockItem): BlockItem = when (blockItem) {
        is Declaration -> {
            val name = blockItem.identifier
            val variables = variables()

            val alreadyDeclaredInScope =
                variables.containsKey(name.identifier) && variables[name.identifier]!!.level == scopes.size

            if (alreadyDeclaredInScope) {
                eu.jameshamilton.frontend.error(name.line, "Duplicate variable declaration '${name.identifier}'.")
            }

            val unique = maketemporary(name)
            variables[name.identifier] = unique

            if (blockItem.initializer == null) {
                Declaration(unique.identifier, null)
            } else {
                Declaration(unique.identifier, resolve(blockItem.initializer))
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
                is InitDecl -> InitDecl(resolve(blockItem.init.declaration) as Declaration)
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

    return scoped {
        FunctionDef(functionDef.name, functionDef.body.map(::resolve))
    }
}
