package eu.jameshamilton.frontend

import eu.jameshamilton.frontend.TokenType.CONSTANT
import eu.jameshamilton.frontend.TokenType.DECREMENT
import eu.jameshamilton.frontend.TokenType.EOF
import eu.jameshamilton.frontend.TokenType.IDENTIFIER
import eu.jameshamilton.frontend.TokenType.INT
import eu.jameshamilton.frontend.TokenType.LEFT_BRACE
import eu.jameshamilton.frontend.TokenType.LEFT_BRACKET
import eu.jameshamilton.frontend.TokenType.LEFT_PAREN
import eu.jameshamilton.frontend.TokenType.MINUS
import eu.jameshamilton.frontend.TokenType.RETURN
import eu.jameshamilton.frontend.TokenType.RIGHT_BRACE
import eu.jameshamilton.frontend.TokenType.RIGHT_BRACKET
import eu.jameshamilton.frontend.TokenType.RIGHT_PAREN
import eu.jameshamilton.frontend.TokenType.SEMICOLON
import eu.jameshamilton.frontend.TokenType.STRING
import eu.jameshamilton.frontend.TokenType.TILDE
import eu.jameshamilton.frontend.TokenType.VOID
import kotlin.system.exitProcess


fun error(line: Int, message: String) {
    System.err.println("[line ${line}] Error${if ("at end".isNotBlank()) " " else ""}${"at end"}: $message")
    exitProcess(-1)
}

fun error(token: Token, message: String) {
    if (token.type == EOF) {
        System.err.println("[line ${token.line}] Error${if ("at end".isNotBlank()) " " else ""}${"at end"}: $message")
    } else {
        val where = "at '${token.lexeme}'"
        System.err.println("[line ${token.line}] Error${if (where.isNotBlank()) " " else ""}${where}: $message")
    }
    exitProcess(-1)
}

class Scanner(private val source: String) {

    private var start = 0
    private var current = 0
    private var line = 1
    private val tokens = mutableListOf<Token>()

    fun scanTokens(): List<Token> {
        while (!isAtEnd()) {
            start = current
            scanToken()
        }

        tokens.add(Token(EOF, "", null, line))
        return tokens
    }

    private fun scanToken() {
        when (val c = advance()) {
            '(' -> addToken(LEFT_PAREN)
            ')' -> addToken(RIGHT_PAREN)
            '{' -> addToken(LEFT_BRACE)
            '}' -> addToken(RIGHT_BRACE)
            '[' -> addToken(LEFT_BRACKET)
            ']' -> addToken(RIGHT_BRACKET)
            ';' -> addToken(SEMICOLON)
            '~' -> addToken(TILDE)
            '-' -> addToken(if (match('-')) DECREMENT else MINUS)
            '"' -> string()
            ' ', '\t', '\r' -> {}
            '\n' -> line++
            else -> {
                if (isDigit(c)) number()
                else if (isAlpha(c)) identifier()
                else error(line, "Unexpected character.")
            }
        }
    }

    private fun identifier() {
        while (isAlphaNumeric(peek())) advance()

        addToken(KEYWORDS.getOrDefault(source.substring(start, current), IDENTIFIER))
    }

    private fun isAlpha(c: Char): Boolean = c in 'a'..'z' || c in 'A'..'Z' || c == '_'
    private fun isDigit(c: Char): Boolean = c in '0'..'9'
    private fun isAlphaNumeric(c: Char) = isAlpha(c) || isDigit(c)

    private fun number() {
        while (isDigit(peek())) advance()

        //if (peek() == '.' && isDigit(peekNext())) {
        //    advance()

        //    while (isDigit(peek())) advance()
        //}

        if (isAlpha(peek())) {
            identifier()
            error(line, "Invalid identifier '${source.substring(start, current)}'.")
            return
        }

        addToken(CONSTANT, source.substring(start, current).toInt())
    }

    private fun string() {
        while (peek() != '"' && !isAtEnd()) {
            if (peek() == '\n') line++
            advance()
        }

        if (isAtEnd()) {
            error(line, "Unterminated string.")
            return
        }

        advance()

        addToken(STRING, (source.substring(start + 1, current - 1)))
    }

    private fun peek(): Char = if (isAtEnd()) 0.toChar() else source[current]

    private fun peekNext(): Char = if (current + 1 >= source.length) 0.toChar() else source[current + 1]

    private fun match(expected: Char): Boolean {
        if (isAtEnd()) return false
        if (source[current] != expected) return false

        current++
        return true
    }

    private fun match(expected: Char, expectedNext: Char): Boolean {
        if (isAtEnd()) return false
        if (peek() == expected && peekNext() == expectedNext) {
            current += 2
            return true
        }

        return false
    }

    private fun addToken(type: TokenType) = addToken(type, null)

    private fun addToken(type: TokenType, literal: Any?) =
        tokens.add(Token(type, source.substring(start, current), literal, line))

    private fun advance(): Char = source[current++]

    private fun isAtEnd() = current >= source.length

    companion object {
        val KEYWORDS: Map<String, TokenType> = mapOf(
            "return" to RETURN,
            "void" to VOID,
            "int" to INT,
        )
    }
}
