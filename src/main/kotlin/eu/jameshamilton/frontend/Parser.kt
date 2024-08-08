package eu.jameshamilton.frontend

import eu.jameshamilton.frontend.BinaryOp.Add
import eu.jameshamilton.frontend.BinaryOp.Divide
import eu.jameshamilton.frontend.BinaryOp.Multiply
import eu.jameshamilton.frontend.BinaryOp.Remainder
import eu.jameshamilton.frontend.BinaryOp.Subtract
import eu.jameshamilton.frontend.TokenType.ASTERISK
import eu.jameshamilton.frontend.TokenType.CONSTANT
import eu.jameshamilton.frontend.TokenType.EOF
import eu.jameshamilton.frontend.TokenType.IDENTIFIER
import eu.jameshamilton.frontend.TokenType.INT
import eu.jameshamilton.frontend.TokenType.LEFT_BRACE
import eu.jameshamilton.frontend.TokenType.LEFT_PAREN
import eu.jameshamilton.frontend.TokenType.MINUS
import eu.jameshamilton.frontend.TokenType.PERCENT
import eu.jameshamilton.frontend.TokenType.PLUS
import eu.jameshamilton.frontend.TokenType.RETURN
import eu.jameshamilton.frontend.TokenType.RIGHT_BRACE
import eu.jameshamilton.frontend.TokenType.RIGHT_PAREN
import eu.jameshamilton.frontend.TokenType.SEMICOLON
import eu.jameshamilton.frontend.TokenType.SLASH
import eu.jameshamilton.frontend.TokenType.TILDE
import eu.jameshamilton.frontend.TokenType.VOID
import eu.jameshamilton.frontend.UnaryOp.Complement
import eu.jameshamilton.frontend.UnaryOp.Negate

class Parser(private val tokens: List<Token>) {
    private var current = 0

    fun parse(): Program {
        return Program(function()).also {
            if (!isAtEnd()) error(previous(), "Expected end of program.")
        }
    }

    private fun function(): FunctionDef {
        val returnType = expect(INT, "Function return type expected.")
        val name = expect(IDENTIFIER, "Function name expected.")
        expect(LEFT_PAREN, "( expected.")
        val parameters = expect(VOID, "Expected void.")
        expect(RIGHT_PAREN, ") expected.")
        expect(LEFT_BRACE, "{ expected.")
        val statements = mutableListOf<Statement>()
        statements.add(statement())
        expect(RIGHT_BRACE, "} expected.")

        return FunctionDef(name, statements)
    }

    private fun statement(): Statement = when {
        match(RETURN) -> ReturnStatement(expression())
        else -> throw error(previous(), "Unexpected statement.")
    }.also {
        expect(SEMICOLON, "Expected semicolon.")
    }


    private fun expression(minPrecedence: Int = 0): Expression {
        fun precedence(op: Token): Int = when (op.type) {
            PLUS -> 45
            MINUS -> 45
            ASTERISK -> 50
            SLASH -> 50
            PERCENT -> 50
            else -> throw error(op, "Unexpected token.")
        }

        var left = factor()
        while (checkAny(PLUS, MINUS, ASTERISK, SLASH, PERCENT)
            && precedence(peek()) >= minPrecedence
        ) {
            val opToken = advance()
            val op = when (opToken.type) {
                PLUS -> Add
                MINUS -> Subtract
                ASTERISK -> Multiply
                SLASH -> Divide
                PERCENT -> Remainder
                else -> throw error(opToken, "Unexpected operator.")
            }
            val right = expression(precedence(opToken) + 1)
            left = BinaryExpr(left, op, right)
        }

        return left
    }

    private fun factor(): Expression = when {
        match(CONSTANT) -> Constant(previous().literal as Int)
        check(TILDE) -> unary()
        check(MINUS) -> unary()
        match(LEFT_PAREN) -> expression().also {
            expect(RIGHT_PAREN, "Expected closing ')' after expression.")
        }

        else -> throw error(previous(), "Unexpected expression.")
    }

    private fun unary(): UnaryExpr = when {
        match(MINUS) -> UnaryExpr(Negate, factor())
        match(TILDE) -> UnaryExpr(Complement, factor())
        else -> throw error(previous(), "Unexpected expression '${peek().literal}'.")
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
            if (previous().type == SEMICOLON) return

            when (peek().type) {
                RETURN -> return
                else -> advance()
            }
        }
        advance()
    }

    private fun error(token: Token, message: String): ParseError {
        eu.jameshamilton.frontend.error(token, message)
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

    private fun checkAny(vararg types: TokenType): Boolean {
        for (type in types) if (check(type)) {
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
        tokens[current + 1].type == EOF -> false
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

    private fun isAtEnd() = peek().type == EOF

    private fun peek(): Token = tokens[current]

    private fun previous(): Token = tokens[current - 1]

    class ParseError : RuntimeException()
}