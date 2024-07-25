package eu.jameshamilton

import kotlinx.cli.ArgParser
import kotlinx.cli.ArgType
import kotlinx.cli.default
import java.io.File
import kotlin.system.exitProcess

val parser = ArgParser("kcc")
val input by parser.argument(ArgType.String, description = "C source code")
val lex by parser.option(ArgType.Boolean, fullName = "lex", description = "Only run the lexer").default(false)
val parse by parser.option(ArgType.Boolean, fullName = "parse", description = "Only run the lexer + parser").default(false)
val codegen by parser.option(ArgType.Boolean, fullName = "codegen", description = "Only run the lexer + parser + codegen").default(false)
val emitAssembly by parser.option(ArgType.Boolean, shortName = "S", description = "Emit assembly").default(false)


fun main(args: Array<String>) {
    parser.parse(args)
    val inputFile = File(input)
    val preprocessed = preprocess(inputFile)
    val compiled = compile(preprocessed)
    if (emitAssembly) {
        File(inputFile.parent, inputFile.nameWithoutExtension + ".s").writeText(compiled.readText())
    } else {
        val assembled = assemble(inputFile.parent, inputFile.nameWithoutExtension, compiled)
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
    return output
}

fun compile(file: File): File {
    val scanner = Scanner(file.readText())
    val tokens = scanner.scanTokens()
    println(tokens)
    if (lex) exitProcess(0)

    val output = File.createTempFile(file.name.removeSuffix(".i"), ".s")
    output.deleteOnExit()
    val r = Runtime.getRuntime().exec(arrayOf("gcc", "-S", file.absolutePath, "-o", output.absolutePath))
    if (r.waitFor() != 0) {
        println(r.errorReader().readText())
        exitProcess(r.exitValue())
    }
    return output
}

fun assemble(outputDirectory: String, outputName: String, input: File): File {
    val output = File(outputDirectory, outputName)
    val r = Runtime.getRuntime().exec(arrayOf("gcc", input.absolutePath, "-o", output.absolutePath))
    if (r.waitFor() != 0) {
        println(r.errorReader().readText())
        exitProcess(r.exitValue())
    }
    return output
}
