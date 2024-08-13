package eu.jameshamilton.frontend

import eu.jameshamilton.frontend.UnaryOp.PostfixDecrement
import eu.jameshamilton.frontend.UnaryOp.PostfixIncrement
import eu.jameshamilton.frontend.UnaryOp.PrefixDecrement
import eu.jameshamilton.frontend.UnaryOp.PrefixIncrement

fun resolve(program: Program): Program = Program(resolve(program.function))

fun resolve(functionDef: FunctionDef): FunctionDef {
    val variables = mutableMapOf<String, Identifier>()

    fun maketemporary(name: Identifier) = Identifier("${name.identifier}.${variables.size}", name.line)

    fun resolve(expression: Expression): Expression = when (expression) {
        is Assignment -> {
            val left = expression.lvalue
            val right = expression.value
            if (left !is Var) {
                error(0, "Expression is not an lvalue.")
            }
            Assignment(resolve(left), resolve(right))
        }

        is Var -> if (expression.identifier.identifier in variables.keys) {
            Var(variables[expression.identifier.identifier]!!)
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
    }

    fun resolve(blockItem: BlockItem): BlockItem = when (blockItem) {
        is Declaration -> {
            val name = blockItem.identifier
            if (name.identifier in variables.keys) {
                error(name.line, "Duplicate variable declaration '${name.identifier}'.")
            }
            val unique = maketemporary(name)
            variables[name.identifier] = unique

            if (blockItem.initializer == null) {
                Declaration(unique)
            } else {
                Declaration(unique, resolve(blockItem.initializer))
            }
        }

        is ExpressionStatement -> ExpressionStatement(resolve(blockItem.expression))
        is ReturnStatement -> ReturnStatement(resolve(blockItem.value))
        else -> blockItem
    }

    return FunctionDef(functionDef.name, functionDef.body.map(::resolve))
}