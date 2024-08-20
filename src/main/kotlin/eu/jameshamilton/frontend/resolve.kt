package eu.jameshamilton.frontend

import eu.jameshamilton.frontend.UnaryOp.PostfixDecrement
import eu.jameshamilton.frontend.UnaryOp.PostfixIncrement
import eu.jameshamilton.frontend.UnaryOp.PrefixDecrement
import eu.jameshamilton.frontend.UnaryOp.PrefixIncrement
import java.util.*

fun resolve(program: Program): Program {
    var resolved = resolveVariables(program)
    resolved = resolveLoopLabels(resolved)
    return resolved
}

private fun resolveVariables(program: Program): Program {
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
        is Label -> Label(blockItem.identifier)
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
    }

    fun resolve(functionDef: FunctionDef): FunctionDef = scoped {
        FunctionDef(functionDef.name, functionDef.body.map(::resolve))
    }

    return Program(resolve(program.function))
}

private fun resolveLoopLabels(program: Program): Program {
    val labels = Stack<Identifier>()
    var counter = 0

    fun makelabel(name: Identifier): Identifier = Identifier("${name.identifier}_${counter++}", name.line)

    fun <T> scoped(block: (label: Identifier) -> T): T = with(makelabel(Identifier("loop", 0))) {
        labels.push(this)
        block(this).also { labels.pop() }
    }

    fun resolve(blockItem: BlockItem): BlockItem = when (blockItem) {
        is DoWhile -> scoped { label ->
            DoWhile(blockItem.condition, resolve(blockItem.body) as Statement, label)
        }

        is While -> scoped { label ->
            While(blockItem.condition, resolve(blockItem.body) as Statement, label)
        }

        is For -> scoped { label ->
            For(blockItem.init, blockItem.condition, blockItem.post, resolve(blockItem.body) as Statement, label)
        }

        is Break -> if (labels.isEmpty()) error("'break' outside of loop.") else Break(labels.peek())
        is Continue -> if (labels.isEmpty()) error("'continue' outside of loop.") else Continue(labels.peek())

        is LabeledStatement -> LabeledStatement(blockItem.identifier, resolve(blockItem.statement) as Statement)
        is Declaration -> Declaration(blockItem.identifier, blockItem.initializer)
        is Label -> Label(blockItem.identifier)
        is Compound -> Compound(blockItem.block.map { resolve(it) })
        is ExpressionStatement -> ExpressionStatement(blockItem.expression)
        is Goto -> Goto(blockItem.identifier)
        is If -> If(blockItem.condition, resolve(blockItem.thenBranch) as Statement,
            blockItem.elseBranch?.let { resolve(it) } as Statement?)

        is ReturnStatement -> ReturnStatement(blockItem.value)
        NullStatement -> NullStatement
    }

    fun resolve(functionDef: FunctionDef): FunctionDef {
        return FunctionDef(functionDef.name, functionDef.body.map(::resolve))
    }

    return Program(resolve(program.function))
}
