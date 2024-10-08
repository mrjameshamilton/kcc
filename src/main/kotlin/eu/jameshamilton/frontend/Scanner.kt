package eu.jameshamilton.frontend

import eu.jameshamilton.frontend.TokenType.AMPERSAND
import eu.jameshamilton.frontend.TokenType.AMPERSAND_EQUAL
import eu.jameshamilton.frontend.TokenType.ASTERISK
import eu.jameshamilton.frontend.TokenType.ASTERISK_EQUAL
import eu.jameshamilton.frontend.TokenType.BREAK
import eu.jameshamilton.frontend.TokenType.CASE
import eu.jameshamilton.frontend.TokenType.CHAR
import eu.jameshamilton.frontend.TokenType.COLON
import eu.jameshamilton.frontend.TokenType.COMMA
import eu.jameshamilton.frontend.TokenType.CONSTANT_CHAR
import eu.jameshamilton.frontend.TokenType.CONSTANT_DOUBLE
import eu.jameshamilton.frontend.TokenType.CONSTANT_INT
import eu.jameshamilton.frontend.TokenType.CONSTANT_LONG
import eu.jameshamilton.frontend.TokenType.CONSTANT_UINT
import eu.jameshamilton.frontend.TokenType.CONSTANT_ULONG
import eu.jameshamilton.frontend.TokenType.CONTINUE
import eu.jameshamilton.frontend.TokenType.DECREMENT
import eu.jameshamilton.frontend.TokenType.DEFAULT
import eu.jameshamilton.frontend.TokenType.DO
import eu.jameshamilton.frontend.TokenType.DOUBLE
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
import eu.jameshamilton.frontend.TokenType.SIGNED
import eu.jameshamilton.frontend.TokenType.SIZEOF
import eu.jameshamilton.frontend.TokenType.SLASH
import eu.jameshamilton.frontend.TokenType.SLASH_EQUAL
import eu.jameshamilton.frontend.TokenType.STATIC
import eu.jameshamilton.frontend.TokenType.STRING
import eu.jameshamilton.frontend.TokenType.SWITCH
import eu.jameshamilton.frontend.TokenType.TILDE
import eu.jameshamilton.frontend.TokenType.UNSIGNED
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

            '.' -> when {
                // double doesn't need a leading 0
                isDigit(peek()) -> double()
                else -> error(line, "Unexpected character '$c'")
            }

            '\'' -> {
                addToken(CONSTANT_CHAR, char(singleChar = true))
                if (!match('\'')) {
                    error(line, "Unterminated character.")
                }
            }

            '"' -> string()

            ' ', '\t', '\r' -> {}
            '\n' -> line++
            '#' -> {
                val isBeginningOfLine = current == 1 || prevprev() == '\n'
                if (isBeginningOfLine) {
                    // https://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html
                    if (match(' ')) {
                        start = current
                        line = integer()
                    }
                    skipToEndOfLine()
                } else {
                    error(line, "Unexpected character '$c'")
                }
            }

            else -> when {
                isDigit(c) -> number()
                isAlpha(c) -> identifier()
                else -> error(line, "Unexpected character '$c'.")
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

    private fun integer(): Int {
        while (isDigit(peek())) advance()
        return source.substring(start, current).toInt()
    }

    private fun number() {
        while (isDigit(peek())) advance()

        if (match('.') || match('e', ignoreCase = true)) {
            double()
            return
        }

        var unsigned = match('u', ignoreCase = true)

        // Use BigInteger for consistency: the parser will
        // check the ranges and convert to the correct type.

        if (match('l', ignoreCase = true)) {
            // long constant
            val endIndex = if (unsigned) current - 2 else current - 1

            @Suppress("ControlFlowWithEmptyBody")
            if (match(prev())) {
                // TODO: long long
            }

            unsigned = unsigned || match('u', ignoreCase = true)

            if (isAlphaNumeric(peek())) {
                error(line, "Unexpected character '${peek()}'.")
            }

            addToken(
                if (unsigned) CONSTANT_ULONG else CONSTANT_LONG,
                source.substring(start, endIndex).toBigInteger()
            )
            return
        }

        if (isAlpha(peek())) {
            identifier()
            error(line, "Invalid identifier '${source.substring(start, current)}'.")
        }

        if (unsigned) {
            addToken(CONSTANT_UINT, source.substring(start, current - 1).toBigInteger())
        } else {
            addToken(CONSTANT_INT, source.substring(start, current).toBigInteger())
        }
    }

    private fun double() {
        while (isDigit(peek())) advance()

        if (match('e', ignoreCase = true)) {
            // consume 'e'
            if (!(isDigit(peek()) || peek() == '+' || peek() == '-')) error(line, "Unexpected character '${peek()}'.")
        }

        if (match('+') || match('-')) {
            // continue
            if (!isDigit(peek())) error(line, "Unexpected character '${peek()}'.")
        }

        while (isDigit(peek())) advance()

        // Catch invalid suffixes.
        if (isAlphaNumeric(peek()) || peek() == '.') error(line, "Unexpected character '${peek()}'.")

        val substring = source.substring(start, current)
        addToken(CONSTANT_DOUBLE, (if (substring.startsWith('.')) "0$substring" else substring).toDouble())
    }

    private fun char(singleChar: Boolean = false): Char = when {
        match('\\') -> when {
            match('\'') -> '\''
            match('"') -> '"'
            match('?') -> '?'
            match('\\') -> '\\'
            match('a') -> '\u0007'
            match('b') -> '\b'
            match('f') -> '\u000C'
            match('n') -> '\n'
            match('r') -> '\r'
            match('t') -> '\t'
            match('v') -> '\u000B'
            else -> error(line, "Invalid escape character: '${peek()}'")
        }

        else -> when {
            singleChar && match('\'') -> error(line, "Single quote character must be escaped.")
            singleChar && match('\n') -> error(line, "Single newline character must be escaped.")
            else -> advance()
        }
    }

    private fun string() {
        val buffer = StringBuffer()
        while (peek() != '"' && peek() != '\n' && !isAtEnd()) {
            buffer.append(char())
        }

        if (!match('\"')) {
            error(line, "Unterminated string.")
        }

        addToken(STRING, buffer.toString())
    }

    private fun peek(): Char = if (isAtEnd()) 0.toChar() else source[current]
    private fun prev(): Char = if (current <= 0) 0.toChar() else source[current - 1]
    private fun prevprev(): Char = if (current - 2 < 0) 0.toChar() else source[current - 2]

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

    private fun addToken(type: TokenType) = addToken(type, null)

    private fun addToken(type: TokenType, literal: Any?) =
        tokens.add(Token(type, source.substring(start, current), literal, line))

    private fun advance(): Char = source[current++]

    private fun isAtEnd() = current >= source.length

    private fun skipToEndOfLine() {
        while (!match('\n')) advance()
    }

    companion object {
        val KEYWORDS: Map<String, TokenType> = mapOf(
            "return" to RETURN,
            "void" to VOID,
            "int" to INT,
            "char" to CHAR,
            "long" to LONG,
            "double" to DOUBLE,
            "signed" to SIGNED,
            "unsigned" to UNSIGNED,
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
            "sizeof" to SIZEOF
        )
    }
}
