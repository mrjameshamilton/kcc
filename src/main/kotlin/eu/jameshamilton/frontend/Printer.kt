package eu.jameshamilton.frontend

import eu.jameshamilton.frontend.check.CharInit
import eu.jameshamilton.frontend.check.ConstantAttr
import eu.jameshamilton.frontend.check.DoubleInit
import eu.jameshamilton.frontend.check.Initial
import eu.jameshamilton.frontend.check.InitialValue
import eu.jameshamilton.frontend.check.IntInit
import eu.jameshamilton.frontend.check.LongInit
import eu.jameshamilton.frontend.check.NoInitializer
import eu.jameshamilton.frontend.check.PointerInit
import eu.jameshamilton.frontend.check.StaticAttr
import eu.jameshamilton.frontend.check.StringInit
import eu.jameshamilton.frontend.check.Tentative
import eu.jameshamilton.frontend.check.UCharInit
import eu.jameshamilton.frontend.check.UIntInit
import eu.jameshamilton.frontend.check.ULongInit
import eu.jameshamilton.frontend.check.ZeroInit
import eu.jameshamilton.frontend.check.symbolTable
import eu.jameshamilton.unreachable
import java.io.PrintStream
import java.util.*

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

fun printVarDeclaration(type: Type, identifier: String = ""): Any = when (type) {
    is ArrayType -> {
        val stack = Stack<ArrayType>().apply { push(type) }
        var baseType = type.element
        while (baseType is ArrayType) {
            stack.push(baseType)
            baseType = baseType.element
        }
        printVarDeclaration(stack.peek().element, identifier)
        os.print(stack.joinToString(separator = "") {
            "[${it.length}]"
        })
    }

    DoubleType -> os.print("double $identifier")
    IntType -> os.print("int $identifier")
    LongType -> os.print("long $identifier")
    is PointerType -> {
        printVarDeclaration(type.referenced)
        os.print("*")
        os.print(identifier)
    }

    UIntType -> os.print("unsigned int $identifier")
    ULongType -> os.print("unsigned long $identifier")
    Unknown -> os.print("???")
    is FunType -> unreachable("cannot use funtype for var declaration")
    CharType -> os.print("char $identifier")
    SCharType -> os.print("signed char $identifier")
    UCharType -> os.print("unsigned char $identifier")
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

        is VarDeclaration -> {
            printVarDeclaration(declaration.type, declaration.name.identifier)
        }
    }
}

fun print(declaration: Declaration) = os.scoped {
    printDefinition(declaration)
    when (declaration) {
        is FunDeclaration -> {
            os.print("(")
            when (declaration.params) {
                null, emptyList<VarDeclaration>() -> os.print("void")
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
            when (val attr = symbolTable[declaration.name.identifier]?.attr) {
                is StaticAttr -> {
                    os.print(" = ")
                    printInitialValue(declaration.type, attr.initialValue)
                }

                else -> if (declaration.initializer != null) {
                    os.print(" = ")
                    printExpression(declaration.initializer)
                }
            }

            os.print(";")
        }
    }
    os.println()
}

fun escape(s: String) = s.map {
    when (it) {
        '\t' -> "\\t"
        '\n' -> "\\n"
        else -> it
    }
}.joinToString("")

fun printInitialValue(type: Type, initialValue: InitialValue) {
    when (initialValue) {
        is Initial -> {
            if (type is ArrayType) os.print("{ ")
            initialValue.value.forEachIndexed { index, it ->
                when (it) {
                    is DoubleInit, is IntInit, is UIntInit, is LongInit, is ULongInit -> os.print(it.value)
                    is ZeroInit -> repeat(it.bytes / type.baseType.sizeInBytes) { os.print(0) }
                    is CharInit -> TODO()
                    is UCharInit -> TODO()
                    is PointerInit -> os.print("\"${(symbolTable[it.value]?.attr as ConstantAttr<*>).staticInit.value}\"")
                    is StringInit -> os.print(""""${escape(it.value)}"""")
                }
                if (index != initialValue.value.lastIndex) os.print(",")
                os.print(" ")
            }
            if (type is ArrayType) os.print("}")
        }

        NoInitializer -> {}
        Tentative -> {}
    }
}

fun printBlockItem(blockItem: BlockItem) {
    when (blockItem) {
        is FunDeclaration -> {
            os.printIndent()
            print(blockItem)
        }

        is VarDeclaration -> {
            os.printIndent()
            print(blockItem)
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
        is DoWhile -> {
            os.printlnIndent("do {")
            os.scoped {
                printBlockItem(blockItem.body)
            }
            os.printIndent("} while (")
            printExpression(blockItem.condition)
            os.println(");")
        }

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

        is StringConstant -> {
            os.print("\"${escape(expression.value)}\"")
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
            os.print("(")
            val isPostfix = expression.op in setOf(UnaryOp.PostfixIncrement, UnaryOp.PostfixDecrement)
            if (!isPostfix) os.print("${expression.op}")
            printExpression(expression.expression)
            if (isPostfix) os.print("${expression.op}")
            os.print(")")
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

        is CompoundInit -> {
            os.print("{ ")
            expression.expressions.forEachIndexed { index, it ->
                printExpression(it)
                if (index != expression.expressions.lastIndex) os.print(",")
                os.print(" ")
            }
            os.print("}")
        }

        is SingleInit -> {
            printExpression(expression.expression)
        }

        is Subscript -> {
            os.print("(")
            printExpression(expression.expr1)
            os.print("[")
            printExpression(expression.expr2)
            os.print("]")
            os.print(")")
        }
    }
}