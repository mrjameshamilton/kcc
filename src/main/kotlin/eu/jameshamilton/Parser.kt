package eu.jameshamilton

import kotlin.math.roundToInt

class Parser(private val tokens: List<Token>) {
    private var current = 0

    fun parse(): Program {
        return Program(function()).also {
           if (!isAtEnd()) error(previous(), "Expected end of program.")
        }
    }

    private fun function(): FunctionDef {
        val returnType = expect(TokenType.INT, "Function return type expected.")
        val name = expect(TokenType.IDENTIFIER, "Function name expected.")
        expect(TokenType.LEFT_PAREN, "( expected.")
        val parameters = expect(TokenType.VOID, "Expected void.")
        expect(TokenType.RIGHT_PAREN, ") expected.")
        expect(TokenType.LEFT_BRACE, "{ expected.")
        val statements = mutableListOf<Statement>()
        statements.add(statement())
        expect(TokenType.RIGHT_BRACE, "} expected.")

        return FunctionDef(name, statements)
    }

    private fun statement(): Statement {
        return when {
            match(TokenType.RETURN) -> ReturnStatement(expression())
            else -> throw error(previous(), "Unexpected statement.")
        }.also {
            expect(TokenType.SEMICOLON, "Expected semicolon.")
        }
    }

    private fun expression(): Expression {
        return when {
            match(TokenType.CONSTANT) -> Constant((previous().literal as Double).roundToInt())
            else -> throw error(previous(), "Unexpected expression '${peek().literal}'.")
        }
    }

    private fun optional(type: TokenType): Token? =
        if (check(type)) advance() else null

    private fun expect(type: TokenType, message: String): Token {
        if (check(type)) return advance()

        throw error(peek(), message)
    }

    private fun synchronize() {
        advance()
        while (!isAtEnd()) {
            if (previous().type == TokenType.SEMICOLON) return

            when (peek().type) {
                TokenType.RETURN -> return
                else -> advance()
            }
        }
        advance()
    }

    private fun error(token: Token, message: String): ParseError {
        eu.jameshamilton.error(token, message)
        return ParseError()
    }

    private fun match(vararg types: TokenType): Boolean {
        for (type in types) if (check(type)) {
            advance()
            return true
        }
        return false
    }

    private fun matchNext(vararg types: TokenType): Boolean {
        for (type in types) if (checkNext(type)) {
            advance()
            return true
        }
        return false
    }

    private fun check(type: TokenType, nextType: TokenType? = null): Boolean = when {
        isAtEnd() -> false
        nextType == null -> peek().type == type
        else -> peek().type == type && checkNext(nextType)
    }

    private fun checkNext(type: TokenType): Boolean = when {
        isAtEnd() -> false
        tokens[current + 1].type == TokenType.EOF -> false
        else -> tokens[current + 1].type == type
    }

    private fun back(): Token {
        if (current > 0) current--
        return tokens[current + 1]
    }

    private fun advance(): Token {
        if (!isAtEnd()) current++
        return previous()
    }

    private fun isAtEnd() = peek().type == TokenType.EOF

    private fun peek(): Token = tokens[current]

    private fun previous(): Token = tokens[current - 1]

    class ParseError : RuntimeException()
}