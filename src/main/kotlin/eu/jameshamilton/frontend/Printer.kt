package eu.jameshamilton.frontend

import java.io.PrintStream

lateinit var os: PrintStream

var indent = -1

fun PrintStream.printlnIndent(o: Any?) {
    print(" ".repeat(indent * 4))
    println(o)
}

fun PrintStream.printIndent(o: Any? = "") {
    print(" ".repeat(indent * 4))
    print(o)
}

private fun PrintStream.scoped(block: PrintStream.() -> Unit) {
    indent++
    block()
    indent--
}

fun printProgram(program: Program, os: PrintStream) {
    eu.jameshamilton.frontend.os = PrintStream(os)
    program.declarations.forEach { print(it) }
    os.close()
}

fun printDefinition(declaration: Declaration) {
    when (declaration.storageClass) {
        StorageClass.NONE -> {}
        StorageClass.STATIC -> os.print("static ")
        StorageClass.EXTERN -> os.print("external ")
    }
    os.print("${declaration.type} ${declaration.name.identifier}")
}

fun print(declaration: Declaration) = os.scoped {
    printDefinition(declaration)
    when (declaration) {
        is FunDeclaration -> {
            os.print("(")
            when (declaration.params) {
                null -> os.print("void")
                else -> declaration.params.forEachIndexed { index, it ->
                    printDefinition(it)
                    if (index != declaration.params.lastIndex) {
                        os.print(", ")
                    }
                }
            }
            os.print(")")

            if (declaration.body == null) {
                print(";")
            } else {
                printlnIndent(" {")
                os.scoped {
                    declaration.body.forEach { printBlockItem(it) }
                }
                printlnIndent("}")
            }
        }

        is VarDeclaration -> {
            if (declaration.initializer != null) {
                os.print(" = ")
                printExpression(declaration.initializer)
            }

            os.print(";")
        }
    }
    os.println()
}

fun printBlockItem(blockItem: BlockItem) {
    when (blockItem) {
        is FunDeclaration -> TODO()
        is VarDeclaration -> {
            os.printIndent()
            printDefinition(blockItem)
            if (blockItem.initializer != null) {
                os.print(" = ")
                printExpression(blockItem.initializer)
            }
            os.println(";")
        }

        is DefaultCase -> TODO()
        is ExpressionCase -> TODO()
        is LabeledStatement -> TODO()
        is Break -> TODO()
        is Compound -> {
            blockItem.block.forEach {
                printBlockItem(it)
            }
        }

        is Continue -> TODO()
        is DoWhile -> TODO()
        is ExpressionStatement -> {
            os.printIndent()
            printExpression(blockItem.expression)
            os.println(";")
        }

        is For -> {
            os.printIndent("for (")
            when (blockItem.init) {
                is InitDecl -> printDefinition(blockItem.init.declaration)
                is InitExpr -> blockItem.init.expression?.let { printExpression(it) }
            }
            os.print("; ")
            blockItem.condition?.let { printExpression(it) }
            os.print("; ")
            blockItem.post?.let { printExpression(it) }
            os.println(") {")
            os.scoped {
                printBlockItem(blockItem.body)
            }
            os.printlnIndent("}")
        }

        is Goto -> TODO()
        is If -> {
            os.printIndent("if (")
            printExpression(blockItem.condition)
            os.println(") {")
            os.scoped {
                printBlockItem(blockItem.thenBranch)
            }
            os.printlnIndent("}")
        }

        NullStatement -> TODO()
        is ReturnStatement -> {
            os.printIndent("return ")
            printExpression(blockItem.value)
            os.println(";")
        }

        is Switch -> TODO()
        is While -> TODO()
    }

}

fun printExpression(expression: Expression) {
    when (expression) {
        is Assignment -> {
            printExpression(expression.lvalue)
            os.print(" = ")
            printExpression(expression.value)
        }

        is BinaryExpr -> {
            os.print("/* type = ${expression.type} */ ")
            printExpression(expression.left)
            os.print(" ${expression.operator} ")
            printExpression(expression.right)
        }

        is Conditional -> TODO()
        is Constant -> os.print(expression.value)
        is FunctionCall -> {
            os.print("/* type = ${expression.type} = */ ${expression.identifier.identifier}(")
            expression.arguments.forEachIndexed { index, expr ->
                printExpression(expr)
                if (index != expression.arguments.lastIndex) {
                    os.print(", ")
                }
            }
            os.print(")")
        }

        is UnaryExpr -> {
            os.print("${expression.op}")
            printExpression(expression.expression)
        }

        is Var -> {
            os.print("/* type = ${expression.type} */ ")
            os.print(expression.identifier.identifier)
        }

        is Cast -> {
            os.print("(${expression.targetType})")
            printExpression(expression.expression)
        }
    }
}