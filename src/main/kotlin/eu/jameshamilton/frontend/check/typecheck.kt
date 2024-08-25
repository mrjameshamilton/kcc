package eu.jameshamilton.frontend.check

import eu.jameshamilton.frontend.Assignment
import eu.jameshamilton.frontend.BinaryExpr
import eu.jameshamilton.frontend.BlockItem
import eu.jameshamilton.frontend.Break
import eu.jameshamilton.frontend.Compound
import eu.jameshamilton.frontend.Conditional
import eu.jameshamilton.frontend.Constant
import eu.jameshamilton.frontend.Continue
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
import eu.jameshamilton.frontend.Switch
import eu.jameshamilton.frontend.UnaryExpr
import eu.jameshamilton.frontend.Var
import eu.jameshamilton.frontend.VarDeclaration
import eu.jameshamilton.frontend.While
import eu.jameshamilton.frontend.error

typealias SymbolTable = HashMap<Identifier, Type>

val symbolTable = SymbolTable()

sealed class Type
data object IntType : Type()
data class FunType(val paramCount: Int?, val defined: Boolean = false) : Type()

fun checktypes(program: Program) {
    program.declarations.forEach { checktypes(it) }
    println(symbolTable)
}

private fun checktypes(functionDeclaration: FunDeclaration) {
    var alreadyDefined = false

    if (functionDeclaration.identifier in symbolTable) {
        val oldDeclaration = symbolTable[functionDeclaration.identifier]
        if (oldDeclaration !is FunType || oldDeclaration.paramCount != functionDeclaration.params?.size) {
            error(
                functionDeclaration.identifier.line,
                "Function '${functionDeclaration.identifier}' previously defined with incompatible type."
            )
        }
        alreadyDefined = oldDeclaration.defined
        if (alreadyDefined && functionDeclaration.body != null) {
            error(
                functionDeclaration.identifier.line,
                "Function '${functionDeclaration.identifier.identifier}' is defined more than once."
            )
        }
    }

    symbolTable[functionDeclaration.identifier] =
        FunType(functionDeclaration.params?.size, alreadyDefined || functionDeclaration.body != null)

    if (functionDeclaration.body != null) {
        functionDeclaration.params?.forEach {
            symbolTable[it] = IntType
        }
        functionDeclaration.body.forEach { checktypes(it) }
    }
}

private fun checktypes(varDeclaration: VarDeclaration) {
    symbolTable[varDeclaration.identifier] = IntType
    varDeclaration.initializer?.let { checktypes(varDeclaration.initializer) }
}

private fun checktypes(expression: Expression): Any? = when (expression) {
    is Assignment -> {
        checktypes(expression.lvalue)
        checktypes(expression.value)
    }

    is BinaryExpr -> {
        checktypes(expression.left)
        checktypes(expression.right)
    }

    is Conditional -> {
        checktypes(expression.condition)
        checktypes(expression.thenBranch)
        checktypes(expression.elseBranch)
    }

    is Constant -> {}
    is FunctionCall -> {
        val identifier = expression.identifier
        val type = symbolTable[identifier]
        if (type !is FunType) {
            error(identifier.line, "'${identifier.identifier}' is not function.")
        }

        if (type.paramCount != null && expression.arguments.size != type.paramCount) {
            error(
                identifier.line,
                "'${identifier.identifier}' called with wrong number of arguments (${expression.arguments.size} found, expected ${type.paramCount})."
            )
        }

        expression.arguments.forEach { checktypes(it) }
    }

    is UnaryExpr -> checktypes(expression.expression)
    is Var -> if (symbolTable[expression.identifier] is FunType) {
        error(expression.identifier.line, "'${expression.identifier}' is a function used as a variable.")
    } else {
    }
}

private fun checktypes(blockItem: BlockItem): Any? = when (blockItem) {
    is FunDeclaration -> checktypes(blockItem)
    is VarDeclaration -> checktypes(blockItem)
    is DefaultCase -> checktypes(blockItem.statement)
    is ExpressionCase -> checktypes(blockItem.statement)
    is LabeledStatement -> checktypes(blockItem.statement)
    is Break -> {}
    is Compound -> blockItem.block.forEach { checktypes(it) }
    is Continue -> {}
    is DoWhile -> {
        checktypes(blockItem.condition)
        checktypes(blockItem.body)
    }

    is ExpressionStatement -> checktypes(blockItem.expression)
    is For -> {
        when (blockItem.init) {
            is InitDecl -> checktypes(blockItem.init.declaration)
            is InitExpr -> blockItem.init.expression?.let { checktypes(it) }
        }
        blockItem.condition?.let { checktypes(it) }
        checktypes(blockItem.body)
        blockItem.post?.let { checktypes(it) }
    }

    is Goto -> {}
    is If -> {
        checktypes(blockItem.condition)
        checktypes(blockItem.thenBranch)
        blockItem.elseBranch?.let { checktypes(it) }
    }

    NullStatement -> {}
    is ReturnStatement -> checktypes(blockItem.value)
    is Switch -> {
        val expression = blockItem.expression
        if (expression is Var) {
            val type = symbolTable[expression.identifier]
            if (type is FunType) {
                error(
                    expression.identifier.line,
                    "Cannot switch on a function; '${expression.identifier}' is a function."
                )
            }
        }
        checktypes(blockItem.statement)
    }

    is While -> {
        checktypes(blockItem.condition)
        checktypes(blockItem.body)
    }
}