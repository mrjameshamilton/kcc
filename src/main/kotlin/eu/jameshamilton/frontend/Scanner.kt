package eu.jameshamilton.frontend

import eu.jameshamilton.frontend.TokenType.AMPERSAND
import eu.jameshamilton.frontend.TokenType.AMPERSAND_EQUAL
import eu.jameshamilton.frontend.TokenType.ASTERISK
import eu.jameshamilton.frontend.TokenType.ASTERISK_EQUAL
import eu.jameshamilton.frontend.TokenType.BREAK
import eu.jameshamilton.frontend.TokenType.CASE
import eu.jameshamilton.frontend.TokenType.COLON
import eu.jameshamilton.frontend.TokenType.COMMA
import eu.jameshamilton.frontend.TokenType.CONSTANT_INT
import eu.jameshamilton.frontend.TokenType.CONSTANT_LONG
import eu.jameshamilton.frontend.TokenType.CONTINUE
import eu.jameshamilton.frontend.TokenType.DECREMENT
import eu.jameshamilton.frontend.TokenType.DEFAULT
import eu.jameshamilton.frontend.TokenType.DO
import eu.jameshamilton.frontend.TokenType.DOUBLE_AMPERSAND
import eu.jameshamilton.frontend.TokenType.DOUBLE_EQUAL
import eu.jameshamilton.frontend.TokenType.DOUBLE_GREATER
import eu.jameshamilton.frontend.TokenType.DOUBLE_GREATER_EQUAL
import eu.jameshamilton.frontend.TokenType.DOUBLE_LESS
import eu.jameshamilton.frontend.TokenType.DOUBLE_LESS_EQUAL
import eu.jameshamilton.frontend.TokenType.DOUBLE_PIPE
import eu.jameshamilton.frontend.TokenType.ELSE
import eu.jameshamilton.frontend.TokenType.EOF
import eu.jameshamilton.frontend.TokenType.EQUAL
import eu.jameshamilton.frontend.TokenType.EXCLAMATION
import eu.jameshamilton.frontend.TokenType.EXCLAMATION_EQUAL
import eu.jameshamilton.frontend.TokenType.EXTERN
import eu.jameshamilton.frontend.TokenType.FOR
import eu.jameshamilton.frontend.TokenType.GOTO
import eu.jameshamilton.frontend.TokenType.GREATER
import eu.jameshamilton.frontend.TokenType.GREATER_EQUAL
import eu.jameshamilton.frontend.TokenType.HAT
import eu.jameshamilton.frontend.TokenType.HAT_EQUAL
import eu.jameshamilton.frontend.TokenType.IDENTIFIER
import eu.jameshamilton.frontend.TokenType.IF
import eu.jameshamilton.frontend.TokenType.INCREMENT
import eu.jameshamilton.frontend.TokenType.INT
import eu.jameshamilton.frontend.TokenType.LEFT_BRACE
import eu.jameshamilton.frontend.TokenType.LEFT_BRACKET
import eu.jameshamilton.frontend.TokenType.LEFT_PAREN
import eu.jameshamilton.frontend.TokenType.LESS
import eu.jameshamilton.frontend.TokenType.LESS_EQUAL
import eu.jameshamilton.frontend.TokenType.LONG
import eu.jameshamilton.frontend.TokenType.MINUS
import eu.jameshamilton.frontend.TokenType.MINUS_EQUAL
import eu.jameshamilton.frontend.TokenType.PERCENT
import eu.jameshamilton.frontend.TokenType.PERCENT_EQUAL
import eu.jameshamilton.frontend.TokenType.PIPE
import eu.jameshamilton.frontend.TokenType.PIPE_EQUAL
import eu.jameshamilton.frontend.TokenType.PLUS
import eu.jameshamilton.frontend.TokenType.PLUS_EQUAL
import eu.jameshamilton.frontend.TokenType.QUESTION
import eu.jameshamilton.frontend.TokenType.RETURN
import eu.jameshamilton.frontend.TokenType.RIGHT_BRACE
import eu.jameshamilton.frontend.TokenType.RIGHT_BRACKET
import eu.jameshamilton.frontend.TokenType.RIGHT_PAREN
import eu.jameshamilton.frontend.TokenType.SEMICOLON
import eu.jameshamilton.frontend.TokenType.SLASH
import eu.jameshamilton.frontend.TokenType.SLASH_EQUAL
import eu.jameshamilton.frontend.TokenType.STATIC
import eu.jameshamilton.frontend.TokenType.STRING
import eu.jameshamilton.frontend.TokenType.SWITCH
import eu.jameshamilton.frontend.TokenType.TILDE
import eu.jameshamilton.frontend.TokenType.VOID
import eu.jameshamilton.frontend.TokenType.WHILE
import kotlin.system.exitProcess

fun error(message: String): Nothing {
    System.err.println("[line unknown] Error: $message")
    exitProcess(-1)
}

fun error(line: Int, message: String): Nothing {
    System.err.println("[line ${line}] Error: $message")
    exitProcess(-1)
}

fun error(token: Token, message: String) {
    if (token.type == EOF) {
        System.err.println("[line ${token.line}] Error: $message")
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
            '-' -> when {
                match('-') -> addToken(DECREMENT)
                match('=') -> addToken(MINUS_EQUAL)
                else -> addToken(MINUS)
            }

            '+' -> when {
                match('+') -> addToken(INCREMENT)
                match('=') -> addToken(PLUS_EQUAL)
                else -> addToken(PLUS)
            }

            '*' -> when {
                match('=') -> addToken(ASTERISK_EQUAL)
                else -> addToken(ASTERISK)
            }

            '/' -> when {
                match('=') -> addToken(SLASH_EQUAL)
                else -> addToken(SLASH)
            }

            '%' -> when {
                match('=') -> addToken(PERCENT_EQUAL)
                else -> addToken(PERCENT)
            }

            '&' -> when {
                match('&') -> addToken(DOUBLE_AMPERSAND)
                match('=') -> addToken(AMPERSAND_EQUAL)
                else -> addToken(AMPERSAND)
            }

            '|' -> when {
                match('|') -> addToken(DOUBLE_PIPE)
                match('=') -> addToken(PIPE_EQUAL)
                else -> addToken(PIPE)
            }

            '^' -> when {
                match('=') -> addToken(HAT_EQUAL)
                else -> addToken(HAT)
            }

            '<' -> when {
                match('<') -> when {
                    match('=') -> addToken(DOUBLE_LESS_EQUAL)
                    else -> addToken(DOUBLE_LESS)
                }

                match('=') -> addToken(LESS_EQUAL)
                else -> addToken(LESS)
            }

            '>' -> when {
                match('>') -> when {
                    match('=') -> addToken(DOUBLE_GREATER_EQUAL)
                    else -> addToken(DOUBLE_GREATER)
                }

                match('=') -> addToken(GREATER_EQUAL)
                else -> addToken(GREATER)
            }

            '=' -> when {
                match('=') -> addToken(DOUBLE_EQUAL)
                else -> addToken(EQUAL)
            }

            '!' -> when {
                match('=') -> addToken(EXCLAMATION_EQUAL)
                else -> addToken(EXCLAMATION)
            }

            '?' -> addToken(QUESTION)

            ':' -> addToken(COLON)

            ',' -> addToken(COMMA)

            '"' -> string()
            ' ', '\t', '\r' -> {}
            '\n' -> line++
            else -> when {
                isDigit(c) -> number()
                isAlpha(c) -> identifier()
                else -> error(line, "Unexpected character.")
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

        // Use BigInteger for consistency: the parser will
        // check the ranges and convert to the correct type.

        if (match('l', 'l') || match('L', 'L')) {
            // long long constant.
            if (peek().lowercaseChar() == 'l') {
                error(line, "Unexpected character '${peek()}'.")
            }
            addToken(CONSTANT_LONG, source.substring(start, current - 2).toBigInteger())
            return
        } else if (match('l') || match('L')) {
            // long constant
            if (peek().lowercaseChar() == 'l') {
                error(line, "Unexpected character '${peek()}'.")
            } else {
                addToken(CONSTANT_LONG, source.substring(start, current - 1).toBigInteger())
            }
            return
        }

        if (isAlpha(peek())) {
            identifier()
            error(line, "Invalid identifier '${source.substring(start, current)}'.")
        }

        try {
            addToken(CONSTANT_INT, source.substring(start, current).toBigInteger())
        } catch (e: NumberFormatException) {
            error(line, "Error parsing number: ${source.substring(start, current)}")
        }
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

    private fun match(expected: Char, ignoreCase: Boolean = false): Boolean {
        if (isAtEnd()) return false
        if (ignoreCase) {
            if (source[current].lowercaseChar() != expected) return false
        } else {
            if (source[current] != expected) return false
        }

        current++
        return true
    }

    private fun match(expected: Char, expectedNext: Char, ignoreCase: Boolean = false): Boolean {
        if (isAtEnd()) return false
        if (ignoreCase) {
            if (peek().lowercaseChar() == expected && peekNext().lowercaseChar() == expectedNext) {
                current += 2
                return true
            }
        } else {
            if (peek() == expected && peekNext() == expectedNext) {
                current += 2
                return true
            }
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
            "long" to LONG,
            "if" to IF,
            "else" to ELSE,
            "goto" to GOTO,
            "do" to DO,
            "while" to WHILE,
            "for" to FOR,
            "break" to BREAK,
            "continue" to CONTINUE,
            "switch" to SWITCH,
            "case" to CASE,
            "default" to DEFAULT,
            "extern" to EXTERN,
            "static" to STATIC,
        )
    }
}
