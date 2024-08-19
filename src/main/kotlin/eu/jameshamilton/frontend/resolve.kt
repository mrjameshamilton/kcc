package eu.jameshamilton.frontend

import eu.jameshamilton.frontend.UnaryOp.PostfixDecrement
import eu.jameshamilton.frontend.UnaryOp.PostfixIncrement
import eu.jameshamilton.frontend.UnaryOp.PrefixDecrement
import eu.jameshamilton.frontend.UnaryOp.PrefixIncrement
import java.util.*

fun resolve(program: Program): Program = Program(resolve(program.function))

fun resolve(functionDef: FunctionDef): FunctionDef {
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
                error(0, "Expression is not an lvalue.")
            }
            Assignment(resolve(left), resolve(right))
        }

        is Var -> if (expression.identifier.identifier in variables().keys) {
            Var(variables()[expression.identifier.identifier]!!.identifier)
        } else {
            error(expression.identifier.line, "Undeclared variable '${expression.identifier.identifier}'.")
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
    }

    fun resolve(blockItem: BlockItem): BlockItem = when (blockItem) {
        is Declaration -> {
            val name = blockItem.identifier
            val variables = variables()

            val alreadyDeclaredInScope =
                variables.containsKey(name.identifier) && variables[name.identifier]!!.level == scopes.size

            if (alreadyDeclaredInScope) {
                error(name.line, "Duplicate variable declaration '${name.identifier}'.")
            }

            val unique = maketemporary(name)
            variables[name.identifier] = unique

            if (blockItem.initializer == null) {
                Declaration(unique.identifier)
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
        is Label -> Label(blockItem.identifier)
    }

    return scoped {
        FunctionDef(functionDef.name, functionDef.body.map(::resolve))
    }
}