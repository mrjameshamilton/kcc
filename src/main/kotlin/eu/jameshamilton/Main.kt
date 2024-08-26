package eu.jameshamilton

import eu.jameshamilton.codegen.convert
import eu.jameshamilton.codegen.emit
import eu.jameshamilton.codegen.replacePseudoRegisters
import eu.jameshamilton.frontend.FunDeclaration
import eu.jameshamilton.frontend.Parser
import eu.jameshamilton.frontend.Scanner
import eu.jameshamilton.frontend.VarDeclaration
import eu.jameshamilton.frontend.check.checklabels
import eu.jameshamilton.frontend.check.checkswitchcases
import eu.jameshamilton.frontend.check.checktypes
import eu.jameshamilton.frontend.error
import eu.jameshamilton.frontend.printProgram
import eu.jameshamilton.frontend.resolve.resolve
import eu.jameshamilton.tacky.convert
import eu.jameshamilton.tacky.printTacky
import kotlinx.cli.ArgParser
import kotlinx.cli.ArgType
import kotlinx.cli.default
import kotlinx.cli.vararg
import java.io.File
import kotlin.system.exitProcess

val parser = ArgParser("kcc")
val input by parser.argument(ArgType.String, description = "C source code").vararg()
val output by parser.option(ArgType.String, shortName = "o")
val lex by parser.option(ArgType.Boolean, fullName = "lex", description = "Only run the lexer").default(false)
val parse by parser.option(ArgType.Boolean, fullName = "parse", description = "Only run the lexer + parser")
    .default(false)
val validate by parser.option(ArgType.Boolean, fullName = "validate").default(false)
val tacky by parser.option(ArgType.Boolean, fullName = "tacky", description = "Only run the lexer + parser + tacky")
    .default(false)
val codegen by parser.option(
    ArgType.Boolean,
    fullName = "codegen",
    description = "Only run the lexer + parser + tacky + codegen"
).default(false)
val emitAssembly by parser.option(ArgType.Boolean, shortName = "S", description = "Emit assembly").default(false)
val emitObject by parser.option(ArgType.Boolean, shortName = "c", description = "Emit object file").default(false)
val printParsed by parser.option(ArgType.Boolean, description = "print-parsed").default(false)
val printPreprocessed by parser.option(ArgType.Boolean, description = "Print the preprocessed output").default(false)
val printTokens by parser.option(ArgType.Boolean, description = "Print tokens").default(false)
val printResolved by parser.option(ArgType.Boolean, description = "Resolved tokens").default(false)
val printTacky by parser.option(ArgType.Boolean, description = "Print tacky").default(false)
val printX86 by parser.option(ArgType.Boolean, description = "Print x86").default(false)
val printAssembly by parser.option(ArgType.Boolean, description = "Print assembly").default(false)
val c23 by parser.option(ArgType.Boolean, "c23").default(false)


fun main(args: Array<String>) {
    parser.parse(args)

    if (input.size > 1 && output == null) {
        error("Multiple input files requires specifying an output file with -o")
    }

    val compiled = input.map {
        val inputFile = File(it)
        val preprocessed = preprocess(inputFile)
        compile(preprocessed)
    }

    val file = File(input.single())
    if (emitAssembly) {
        val outputFile = if (output == null) {
            File(file.parentFile, file.nameWithoutExtension + ".s")
        } else {
            File(output!!)
        }

        compiled.forEach { c ->
            outputFile.appendText(c.readText())
        }
    } else {
        val assembled = if (output == null) {
            val outputFile = File(file.parentFile, file.nameWithoutExtension + (if (emitObject) ".o" else ""))
            assemble(outputFile, compiled)
        } else {
            val outputFile = File(output!!)
            assemble(outputFile, compiled)
        }
    }
}

fun preprocess(file: File): File {
    val output = File.createTempFile(file.name.removeSuffix(".c"), ".i")
    output.deleteOnExit()
    val r = Runtime.getRuntime().exec(arrayOf("gcc", "-E", "-P", file.absolutePath, "-o", output.absolutePath))
    if (r.waitFor() != 0) {
        println(r.errorReader().readText())
        exitProcess(r.exitValue())
    }
    if (printPreprocessed) {
        println(output.readText())
    }
    return output
}

fun compile(file: File): File {
    val scanner = Scanner(file.readText())
    val tokens = scanner.scanTokens()
    if (printTokens) println(tokens)
    if (lex) exitProcess(0)
    val parser = Parser(tokens)
    val parsed = parser.parse()
    if (parse) exitProcess(0)
    if (printParsed) {
        //printProgram(parsed, System.out)
        parsed.declarations.forEach {
            println(it.name)
            when (it) {
                is FunDeclaration -> it.body?.forEach { println(it) }
                is VarDeclaration -> println(it)
            }
        }
    }
    val resolved = resolve(parsed)
    checktypes(resolved)
    checklabels(parsed)
    checkswitchcases(parsed)
    if (printResolved) {
        resolved.declarations.forEach {
            when (it) {
                is FunDeclaration -> it.body?.forEach { println(it) }
                is VarDeclaration -> println(it)
            }
        }
    }
    if (validate) exitProcess(0)
    val tackye = convert(resolved)
    if (printTacky) printTacky(tackye)
    if (tacky) exitProcess(0)
    val x86AST = convert(tackye)
    if (printX86) println(x86AST)
    val reg = replacePseudoRegisters(x86AST)
    if (printX86) println(reg)
    if (codegen) exitProcess(0)

    val output = File.createTempFile(file.name.removeSuffix(".i"), ".s")
    output.deleteOnExit()
    val text = emit(reg)
    output.writeText(text)
    if (printAssembly) println(text)
    return output
}

fun assemble(output: File, input: List<File>): File {
    val r = Runtime.getRuntime()
        .exec(arrayOf("gcc") +
                (if (emitObject) arrayOf("-c") else emptyArray()) +
                input.map { it.absolutePath } +
                arrayOf("-o", output.absolutePath)
        )

    if (r.waitFor() != 0) {
        println(r.errorReader().readText())
        exitProcess(r.exitValue())
    }
    return output
}
