package eu.jameshamilton.frontend.check

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
import eu.jameshamilton.frontend.DoWhile
import eu.jameshamilton.frontend.Expression
import eu.jameshamilton.frontend.ExpressionCase
import eu.jameshamilton.frontend.ExpressionStatement
import eu.jameshamilton.frontend.For
import eu.jameshamilton.frontend.FunDeclaration
import eu.jameshamilton.frontend.FunType
import eu.jameshamilton.frontend.FunctionCall
import eu.jameshamilton.frontend.Goto
import eu.jameshamilton.frontend.If
import eu.jameshamilton.frontend.InitDecl
import eu.jameshamilton.frontend.InitExpr
import eu.jameshamilton.frontend.IntType
import eu.jameshamilton.frontend.LabeledStatement
import eu.jameshamilton.frontend.NullStatement
import eu.jameshamilton.frontend.Program
import eu.jameshamilton.frontend.ReturnStatement
import eu.jameshamilton.frontend.StorageClass
import eu.jameshamilton.frontend.StorageClass.EXTERN
import eu.jameshamilton.frontend.StorageClass.STATIC
import eu.jameshamilton.frontend.Switch
import eu.jameshamilton.frontend.Type
import eu.jameshamilton.frontend.UnaryExpr
import eu.jameshamilton.frontend.Var
import eu.jameshamilton.frontend.VarDeclaration
import eu.jameshamilton.frontend.While
import eu.jameshamilton.frontend.error

typealias SymbolTable = HashMap<String, SymbolTableEntry>

data class SymbolTableEntry(val type: Type, val attr: IdentifierAttr? = null)

val symbolTable = SymbolTable()

sealed class IdentifierAttr
data class FunAttr(val defined: Boolean, val global: Boolean) : IdentifierAttr()
data class StaticAttr(val initialValue: InitialValue, val global: Boolean) : IdentifierAttr()
data object LocalAttr : IdentifierAttr()
sealed class InitialValue
data object Tentative : InitialValue()
data class Initial(val value: Int) : InitialValue()
data object NoInitializer : InitialValue()


fun checktypes(program: Program) {
    program.declarations.forEach { checkfilescope(it) }
}

private fun checkfilescope(declaration: Declaration) = when (declaration) {
    is FunDeclaration -> checktypes(declaration)
    is VarDeclaration -> checkfilescope(declaration)
}

private fun checktypes(functionDeclaration: FunDeclaration) {
    var alreadyDefined = false
    // static functions will not be globally visible.
    var global = functionDeclaration.storageClass != STATIC

    if (functionDeclaration.name.identifier in symbolTable) {
        val oldDeclaration = symbolTable[functionDeclaration.name.identifier]!!

        if (oldDeclaration.type !is FunType) {
            error(
                functionDeclaration.name.line,
                "Function '${functionDeclaration.name}' previously defined as ${oldDeclaration.type}."
            )
        }

        if (oldDeclaration.type.paramsTypes.size != functionDeclaration.params?.size) {
            error(
                functionDeclaration.name.line,
                "Function '${functionDeclaration.name}' previously defined with incompatible type."
            )
        }

        val oldAttr = oldDeclaration.attr
        require(oldAttr is FunAttr)

        alreadyDefined = oldAttr.defined
        if (alreadyDefined && functionDeclaration.body != null) {
            error(
                functionDeclaration.name.line,
                "Function '${functionDeclaration.name.identifier}' is defined more than once."
            )
        }

        if (oldAttr.global && functionDeclaration.storageClass == STATIC) {
            error(
                functionDeclaration.name.line,
                "Function '${functionDeclaration.name}' previously declared as non-static."
            )
        }

        global = oldAttr.global
    }

    val type = FunType(functionDeclaration.params?.map { it.type }.orEmpty(), functionDeclaration.type)
    val attr = FunAttr(alreadyDefined || functionDeclaration.body != null, global)
    symbolTable[functionDeclaration.name.identifier] = SymbolTableEntry(type, attr)

    if (functionDeclaration.body != null) {
        functionDeclaration.params?.forEach {
            val type = IntType
            symbolTable[it.name.identifier] = SymbolTableEntry(type, LocalAttr)
        }
        functionDeclaration.body.forEach { checktypes(it) }
    }
}

private fun checklocalscope(varDeclaration: VarDeclaration) {
    when (varDeclaration.storageClass) {
        EXTERN -> {
            if (varDeclaration.initializer != null) {
                error(
                    varDeclaration.name.line,
                    "Local variable '${varDeclaration.name}' with 'extern' storage cannot have initializer."
                )
            }

            if (varDeclaration.name.identifier in symbolTable) {
                val oldDeclaration = symbolTable[varDeclaration.name.identifier]!!
                if (oldDeclaration.type != IntType) {
                    error(varDeclaration.name.line, "Variable '${varDeclaration.name}' previously defined as function.")
                }
            } else {
                val type = IntType
                val attrs = StaticAttr(NoInitializer, true)
                symbolTable[varDeclaration.name.identifier] = SymbolTableEntry(type, attrs)
            }
        }

        STATIC -> {
            val initialValue = when (varDeclaration.initializer) {
                is Constant -> Initial(varDeclaration.initializer.value as Int)
                null -> Initial(0)
                else -> error(
                    varDeclaration.name.line,
                    "Variable '${varDeclaration.name}' is local and cannot have a non-constant initializer."
                )
            }

            val type = IntType
            val attrs = StaticAttr(initialValue, false)
            symbolTable[varDeclaration.name.identifier] = SymbolTableEntry(type, attrs)
        }

        else -> {
            val type = IntType
            symbolTable[varDeclaration.name.identifier] = SymbolTableEntry(type, LocalAttr)
            varDeclaration.initializer?.let { checktypes(varDeclaration.initializer) }
        }
    }
}

private fun checkfilescope(varDeclaration: VarDeclaration) {
    var initialValue = when (varDeclaration.initializer) {
        is Constant -> Initial(varDeclaration.initializer.value as Int)
        null -> when (varDeclaration.storageClass) {
            EXTERN -> NoInitializer
            else -> Tentative
        }

        else -> error(
            varDeclaration.name.line,
            "Static variable '${varDeclaration.name}' must have constant initializer."
        )
    }

    var global = varDeclaration.storageClass != STATIC

    if (varDeclaration.name.identifier in symbolTable) {
        val oldDeclaration = symbolTable[varDeclaration.name.identifier]!!

        if (oldDeclaration.type != IntType) {
            error(varDeclaration.name.line, "Variable '${varDeclaration.name}' previously defined as function.")
        }

        if (oldDeclaration.attr is StaticAttr) {
            if (varDeclaration.storageClass == EXTERN) {
                global = oldDeclaration.attr.global
            } else if (oldDeclaration.attr.global != global) {
                error(varDeclaration.name.line, "Variable '${varDeclaration.name}' has conflicting linkage.")
            }

            if (oldDeclaration.attr.initialValue is Initial) {
                if (initialValue is Initial) {
                    error(
                        varDeclaration.name.line,
                        "Multiple declarations of '${varDeclaration.name.identifier}' have initializers."
                    )
                } else {
                    initialValue = oldDeclaration.attr.initialValue
                }
            } else if (initialValue !is Initial && oldDeclaration.attr.initialValue is Tentative) {
                initialValue = Tentative
            }
        }
    }

    val type = IntType
    val attrs = StaticAttr(initialValue, global)
    symbolTable[varDeclaration.name.identifier] = SymbolTableEntry(type, attrs)
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

        if (identifier.identifier !in symbolTable) {
            error(identifier.line, "'${identifier.identifier}' is not defined.")
        }

        val type = symbolTable[identifier.identifier]!!.type
        if (type !is FunType) {
            error(identifier.line, "'${identifier.identifier}' is not function.")
        }

        if (expression.arguments.size != type.paramsTypes.size) {
            error(
                identifier.line,
                "'${identifier.identifier}' called with wrong number of arguments (${expression.arguments.size} found, expected ${type.paramsTypes.size})."
            )
        }

        expression.arguments.forEach { checktypes(it) }
    }

    is UnaryExpr -> checktypes(expression.expression)
    is Var -> if (symbolTable[expression.identifier.identifier]?.type is FunType) {
        error(expression.identifier.line, "'${expression.identifier}' is a function used as a variable.")
    } else {
    }

    is Cast -> checktypes(expression.expression)
}

private fun checktypes(blockItem: BlockItem): Any? = when (blockItem) {
    is FunDeclaration -> checktypes(blockItem)
    is VarDeclaration -> checklocalscope(blockItem)
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
            is InitDecl -> {
                if (blockItem.init.declaration.storageClass != StorageClass.NONE) {
                    error(0, "For initializer cannot have storage specifiers.")
                }

                checklocalscope(blockItem.init.declaration)
            }

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
            val type = symbolTable[expression.identifier.identifier]?.type
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