package eu.jameshamilton.tacky

import eu.jameshamilton.frontend.FunType
import eu.jameshamilton.frontend.check.symbolTable
import eu.jameshamilton.tacky.BinaryOp.Add
import eu.jameshamilton.tacky.BinaryOp.And
import eu.jameshamilton.tacky.BinaryOp.Divide
import eu.jameshamilton.tacky.BinaryOp.Equal
import eu.jameshamilton.tacky.BinaryOp.GreaterThan
import eu.jameshamilton.tacky.BinaryOp.GreaterThanOrEqual
import eu.jameshamilton.tacky.BinaryOp.LeftShift
import eu.jameshamilton.tacky.BinaryOp.LessThan
import eu.jameshamilton.tacky.BinaryOp.LessThanOrEqual
import eu.jameshamilton.tacky.BinaryOp.Multiply
import eu.jameshamilton.tacky.BinaryOp.NotEqual
import eu.jameshamilton.tacky.BinaryOp.Or
import eu.jameshamilton.tacky.BinaryOp.Remainder
import eu.jameshamilton.tacky.BinaryOp.RightShift
import eu.jameshamilton.tacky.BinaryOp.Subtract
import eu.jameshamilton.tacky.BinaryOp.Xor


fun printTacky(program: Program) {
    val staticVariables = program.items.filterIsInstance<StaticVariable>()
    val functionDefs = program.items.filterIsInstance<FunctionDef>()
    staticVariables.forEach {
        printTacky(it)
    }
    if (staticVariables.isNotEmpty()) {
        println()
    }
    functionDefs.forEach {
        printTacky(it)
    }
}

private fun printTacky(staticVariable: StaticVariable) {
    if (staticVariable.global) print("global ")
    print("${staticVariable.type} ")
    print(staticVariable.name)
    print(" = ")
    print(staticVariable.init)
    println()
}

private fun printTacky(functionDef: FunctionDef) {
    if (functionDef.global) print("global ")

    val funType = symbolTable[functionDef.name]?.type as FunType?
    funType?.let { print("${it.returnType} ") }
    print(functionDef.name)
    println(
        "(${functionDef.parameters.joinToString(", ") { "${it.second} ${it.first}" }}) {"
    )
    println("  entry:")
    functionDef.instructions.forEach {
        printTacky(it)
    }
    println("}")
}

private fun printTacky(instruction: Instruction) {
    when (instruction) {
        is Binary -> {
            val op = when (instruction.op) {
                Add -> "+"
                Subtract -> "-"
                Multiply -> "*"
                Divide -> "/"
                Remainder -> "%"
                And -> "&"
                Or -> "|"
                Xor -> "^"
                LeftShift -> "<<"
                RightShift -> ">>"
                Equal -> "=="
                NotEqual -> "!="
                LessThan -> "<"
                LessThanOrEqual -> "<="
                GreaterThan -> ">"
                GreaterThanOrEqual -> ">="
            }
            print("    ")
            printTacky(instruction.dst, printType = false)
            print(" = ")
            printTacky(instruction.src1)
            print(" $op ")
            printTacky(instruction.src2)
            println()
        }

        is Copy -> {
            print("    ")
            printTacky(instruction.dst, printType = false)
            print(" = ")
            printTacky(instruction.src)
            println()
        }

        is FunctionCall -> {
            print("    ")
            printTacky(instruction.dst, printType = false)
            print(" = ")
            print(instruction.name + "(")
            instruction.arguments.forEachIndexed { index, arg ->
                printTacky(arg)
                if (index != instruction.arguments.lastIndex) print(", ")
            }
            println(")")
        }

        is Jump -> println("    jmp ${instruction.target}")
        is JumpIfNotZero -> {
            print("    jne ")
            printTacky(instruction.condition)
            println(" ${instruction.target}")
        }

        is JumpIfZero -> {
            print("    jeq ")
            printTacky(instruction.condition)
            println(" ${instruction.target}")
        }

        is Label -> println("  ${instruction.identifier}:")
        is Return -> {
            print("    ret ")
            printTacky(instruction.value)
            println()
        }

        is Unary -> {
            val op = when (instruction.op) {
                UnaryOp.Complement -> "~"
                UnaryOp.Negate -> "-"
                UnaryOp.Not -> "!"
            }
            print("    ")
            printTacky(instruction.dst, printType = false)
            print(" = $op")
            printTacky(instruction.src)
            println()
        }

        is SignExtend -> {
            print("    ")
            printTacky(instruction.dst, printType = false)
            print(" = sext ")
            printTacky(instruction.src)
            println()
        }

        is ZeroExtend -> {
            print("    ")
            printTacky(instruction.dst, printType = false)
            print(" = zext ")
            printTacky(instruction.src)
            println()
        }

        is Truncate -> {
            print("    ")
            printTacky(instruction.dst, printType = false)
            print(" = trunc ")
            printTacky(instruction.src)
            println()
        }

    }
}

private fun printTacky(value: Value, printType: Boolean = true) {
    when (value) {
        is Constant -> when (value.value) {
            is Int -> print("int ${value.value}")
            is Long -> print("long ${value.value}")
        }

        is Var -> if (printType) {
            print("${value.type} $${value.name}")
        } else {
            print("$${value.name}")
        }
    }
}
