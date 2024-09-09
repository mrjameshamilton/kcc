package eu.jameshamilton.frontend.resolve

import eu.jameshamilton.frontend.AddrOf
import eu.jameshamilton.frontend.Assignable
import eu.jameshamilton.frontend.Assignment
import eu.jameshamilton.frontend.BinaryExpr
import eu.jameshamilton.frontend.BlockItem
import eu.jameshamilton.frontend.Break
import eu.jameshamilton.frontend.Cast
import eu.jameshamilton.frontend.Compound
import eu.jameshamilton.frontend.Conditional
import eu.jameshamilton.frontend.Constant
import eu.jameshamilton.frontend.Continue
import eu.jameshamilton.frontend.Declaration
import eu.jameshamilton.frontend.DefaultCase
import eu.jameshamilton.frontend.Dereference
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
import eu.jameshamilton.frontend.StorageClass
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
import java.util.*

private data class Variable(val identifier: Identifier, val hasLinkage: Boolean = false, val level: Int = 0)

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

private var functionCounter = 0

fun resolveVariables(program: Program): Program = scoped {
    Program(program.declarations.map { resolveFileScope(it) })
}

private fun resolveFileScope(declaration: Declaration) = when (declaration) {
    is FunDeclaration -> {
        functionCounter++
        resolveFileOrBlockScope(declaration)
    }

    is VarDeclaration -> {
        var hasLinkage = true

        // At file scope: if an identifier with extern is declared at a point
        // where a prior declaration is visible, and the prior
        // declaration has linkage, then the linkage is the same
        // as the previous.
        //
        // static int a; // internal linkage
        // extern int a; // external linkage declared BUT it will actually
        //               // have internal linkage due to the previous declaration.
        //
        if (declaration.storageClass == StorageClass.EXTERN &&
            variables.containsKey(declaration.name.identifier) &&
            variables[declaration.name.identifier]!!.hasLinkage
        ) {
            hasLinkage = variables[declaration.name.identifier]!!.hasLinkage
        }

        val newName = Variable(declaration.name, hasLinkage, scopes.size)
        variables[declaration.name.identifier] = newName
        VarDeclaration(newName.identifier, declaration.initializer, declaration.type, declaration.storageClass)
    }
}

private fun resolveFileOrBlockScope(funDeclaration: FunDeclaration): FunDeclaration {
    // File scope and block scope functions can be treated the same.
    // Block scope functions can't have definitions which is checked in resolve(blockItem).
    val name = resolve(funDeclaration, hasLinkage = true)

    return scoped {
        val arguments = funDeclaration.params?.map {
            val resolvedParam = resolve(it, hasLinkage = false)
            VarDeclaration(resolvedParam.identifier, it.type, StorageClass.NONE)
        }
        val body = funDeclaration.body?.map { resolve(it) }
        FunDeclaration(name.identifier, arguments, body, funDeclaration.type, funDeclaration.storageClass)
    }
}

private fun <T : Declaration> resolve(declaration: T, hasLinkage: Boolean): Variable {
    val identifier = declaration.name.identifier

    val alreadyDeclaredInScope =
        variables.containsKey(identifier) &&
                variables[identifier]?.level == scopes.size

    if (alreadyDeclaredInScope) {
        val previousVariable = variables[identifier]!!

        // External linkage declarations can be declared multiple times.
        if (previousVariable.hasLinkage && hasLinkage) {
            return previousVariable
        }

        if (declaration is VarDeclaration) {
            if (hasLinkage != previousVariable.hasLinkage) {
                error(declaration.name.line, "Duplicate declaration of '$identifier' with different linkage.")
            } else {
                error(declaration.name.line, "Duplicate declaration of '$identifier'.")
            }
        }
    }

    val newName = if (declaration is FunDeclaration || declaration.storageClass == StorageClass.EXTERN) {
        declaration.name.identifier
    } else {
        "${declaration.name.identifier}_${functionCounter}.${scopes.size}.${variables.size}"
    }

    return Variable(Identifier(newName, declaration.name.line), hasLinkage, scopes.size).also {
        variables[identifier] = it
    }
}

private fun resolve(expression: Expression): Expression = when (expression) {
    is Assignment -> {
        val left = expression.lvalue
        val right = expression.rvalue
        Assignment(resolve(left), resolve(right))
    }

    is Var -> if (expression.identifier.identifier in variables) {
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
                if (expression.expression !is Assignable) {
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

    is Cast -> Cast(expression.targetType, resolve(expression.expression))
    is AddrOf -> AddrOf(resolve(expression.expression))
    is Dereference -> Dereference(resolve(expression.expression))
}

private fun resolve(blockItem: BlockItem): BlockItem = when (blockItem) {
    is Declaration -> {
        val name = resolve(blockItem, hasLinkage = blockItem.storageClass == StorageClass.EXTERN)

        when (blockItem) {
            is VarDeclaration -> {
                if (blockItem.initializer == null) {
                    VarDeclaration(name.identifier, blockItem.type, blockItem.storageClass)
                } else {
                    VarDeclaration(
                        name.identifier,
                        resolve(blockItem.initializer),
                        blockItem.type,
                        blockItem.storageClass
                    )
                }
            }

            is FunDeclaration -> {
                if (blockItem.body != null) {
                    error(
                        blockItem.name.line,
                        "Nested function '${blockItem.name.identifier}' definition not allowed."
                    )
                }

                if (blockItem.storageClass == StorageClass.STATIC) {
                    error(
                        blockItem.name.line,
                        "Function declaration '${blockItem.name.identifier}' cannot have 'static' specifier."
                    )
                }

                resolveFileOrBlockScope(blockItem)
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
