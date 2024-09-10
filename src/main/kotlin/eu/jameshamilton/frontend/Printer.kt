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
    //os.close()
}

fun printDefinition(declaration: Declaration) {
    when (declaration.storageClass) {
        StorageClass.NONE -> {}
        StorageClass.STATIC -> os.print("static ")
        StorageClass.EXTERN -> os.print("external ")
    }
    when (declaration) {
        is FunDeclaration ->
            os.print("${(declaration.type as FunType?)?.returnType} ${declaration.name.identifier}")

        is VarDeclaration ->
            os.print("${declaration.type} ${declaration.name.identifier}")
    }
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
        is FunDeclaration -> {
            os.printIndent()
            print(blockItem)
        }

        is VarDeclaration -> {
            os.printIndent()
            printDefinition(blockItem)
            if (blockItem.initializer != null) {
                os.print(" = ")
                printExpression(blockItem.initializer)
            }
            os.println(";")
        }

        is DefaultCase -> {
            os.printlnIndent("default:")
            os.scoped {
                printBlockItem(blockItem.statement)
            }
        }

        is ExpressionCase -> {
            os.printIndent("case ")
            printExpression(blockItem.expression)
            os.println(":")
            os.scoped {
                printBlockItem(blockItem.statement)
            }
        }

        is LabeledStatement -> {
            os.printlnIndent("${blockItem.identifier}: ")
            printBlockItem(blockItem.statement)
        }

        is Break -> os.printlnIndent("break ${blockItem.identifier}")
        is Compound -> {
            blockItem.block.forEach {
                printBlockItem(it)
            }
        }

        is Continue -> os.printlnIndent("continue ${blockItem.identifier}")
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

        is Goto -> os.printlnIndent("goto ${blockItem.identifier};")
        is If -> {
            os.printIndent("if (")
            printExpression(blockItem.condition)
            os.println(") {")
            os.scoped {
                printBlockItem(blockItem.thenBranch)
            }
            os.printlnIndent("}")
        }

        NullStatement -> os.print(";")
        is ReturnStatement -> {
            os.printIndent("return ")
            printExpression(blockItem.value)
            os.println(";")
        }

        is Switch -> {
            os.printIndent("switch (")
            printExpression(blockItem.expression)
            os.println(") {")
            os.scoped {
                printBlockItem(blockItem.statement)
            }
            os.printlnIndent("}")
        }

        is While -> {
            os.printIndent("while (")
            printExpression(blockItem.condition)
            os.println(") {")
            os.scoped {
                printBlockItem(blockItem.body)
            }
            os.printlnIndent("}")
        }
    }

}

fun printExpression(expression: Expression) {
    when (expression) {
        is Assignment -> {
            printExpression(expression.lvalue)
            os.print(" = ")
            printExpression(expression.rvalue)
        }

        is BinaryExpr -> {
            //os.print("/* type = ${expression.type} */ ")
            printExpression(expression.left)
            os.print(" ${expression.operator} ")
            printExpression(expression.right)
        }

        is Conditional -> {
            printExpression(expression.condition)
            os.print(" ? ")
            printExpression(expression.thenBranch)
            os.print(" : ")
            printExpression(expression.elseBranch)
        }

        is Constant -> {
            // os.print("/* type = ${expression.type} */ ")
            os.print("${expression.value}")
        }

        is FunctionCall -> {
            //  os.print("/* type = ${expression.type} = */ ")
            os.print("${expression.identifier.identifier}(")
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
            // os.print("/* type = ${expression.type} */ ")
            os.print(expression.identifier.identifier)
        }

        is Cast -> {
            os.print("(${expression.targetType})")
            printExpression(expression.expression)
        }

        is AddrOf -> {
            os.print("&")
            printExpression(expression.expression)
        }

        is Dereference -> {
            os.print("*")
            printExpression(expression.expression)
        }
    }
}