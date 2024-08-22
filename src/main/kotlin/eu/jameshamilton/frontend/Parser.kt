package eu.jameshamilton.frontend

import eu.jameshamilton.c23
import eu.jameshamilton.frontend.BinaryOp.Add
import eu.jameshamilton.frontend.BinaryOp.And
import eu.jameshamilton.frontend.BinaryOp.Divide
import eu.jameshamilton.frontend.BinaryOp.Equal
import eu.jameshamilton.frontend.BinaryOp.GreaterThan
import eu.jameshamilton.frontend.BinaryOp.GreaterThanOrEqual
import eu.jameshamilton.frontend.BinaryOp.LeftShift
import eu.jameshamilton.frontend.BinaryOp.LessThan
import eu.jameshamilton.frontend.BinaryOp.LessThanOrEqual
import eu.jameshamilton.frontend.BinaryOp.LogicalAnd
import eu.jameshamilton.frontend.BinaryOp.LogicalOr
import eu.jameshamilton.frontend.BinaryOp.Multiply
import eu.jameshamilton.frontend.BinaryOp.NotEqual
import eu.jameshamilton.frontend.BinaryOp.Or
import eu.jameshamilton.frontend.BinaryOp.Remainder
import eu.jameshamilton.frontend.BinaryOp.RightShift
import eu.jameshamilton.frontend.BinaryOp.Subtract
import eu.jameshamilton.frontend.BinaryOp.Xor
import eu.jameshamilton.frontend.TokenType.AMPERSAND
import eu.jameshamilton.frontend.TokenType.AMPERSAND_EQUAL
import eu.jameshamilton.frontend.TokenType.ASTERISK
import eu.jameshamilton.frontend.TokenType.ASTERISK_EQUAL
import eu.jameshamilton.frontend.TokenType.BREAK
import eu.jameshamilton.frontend.TokenType.CASE
import eu.jameshamilton.frontend.TokenType.COLON
import eu.jameshamilton.frontend.TokenType.CONSTANT
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
import eu.jameshamilton.frontend.TokenType.LEFT_PAREN
import eu.jameshamilton.frontend.TokenType.LESS
import eu.jameshamilton.frontend.TokenType.LESS_EQUAL
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
import eu.jameshamilton.frontend.TokenType.RIGHT_PAREN
import eu.jameshamilton.frontend.TokenType.SEMICOLON
import eu.jameshamilton.frontend.TokenType.SLASH
import eu.jameshamilton.frontend.TokenType.SLASH_EQUAL
import eu.jameshamilton.frontend.TokenType.SWITCH
import eu.jameshamilton.frontend.TokenType.TILDE
import eu.jameshamilton.frontend.TokenType.VOID
import eu.jameshamilton.frontend.TokenType.WHILE
import eu.jameshamilton.frontend.UnaryOp.Complement
import eu.jameshamilton.frontend.UnaryOp.Negate
import eu.jameshamilton.frontend.UnaryOp.Not
import eu.jameshamilton.frontend.UnaryOp.PostfixDecrement
import eu.jameshamilton.frontend.UnaryOp.PostfixIncrement
import eu.jameshamilton.frontend.UnaryOp.PrefixDecrement
import eu.jameshamilton.frontend.UnaryOp.PrefixIncrement
import eu.jameshamilton.unreachable

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
        val body = block()
        return FunctionDef(Identifier(name.lexeme, previous().line), body)
    }

    private fun blockItem(): BlockItem = when {
        check(INT) -> declaration()
        c23 && check(IDENTIFIER, COLON) -> {
            // C23 labels without statement: https://www.open-std.org/jtc1/sc22/wg14/www/docs/n2508.pdf
            val identifier = expect(IDENTIFIER, "Identifier expected.")
            expect(COLON, "Expected colon.")
            LabeledStatement(Identifier(identifier.lexeme, previous().line), NullStatement)
        }

        else -> statement()
    }

    private fun statement(): Statement = when {
        match(RETURN) -> ReturnStatement(expression()).also {
            expect(SEMICOLON, "Expected semicolon.")
        }

        check(SEMICOLON) -> NullStatement.also {
            expect(SEMICOLON, "Expected semicolon.")
        }

        match(IF) -> ifStatement()
        check(LEFT_BRACE) -> Compound(block())
        match(GOTO) -> {
            val identifier = expect(IDENTIFIER, "Expected identifier.")
            Goto(Identifier(identifier.lexeme, previous().line)).also {
                expect(SEMICOLON, "Expected semicolon.")
            }
        }

        check(IDENTIFIER, COLON) -> {
            val identifier = expect(IDENTIFIER, "Expected identifier.")
            expect(COLON, "Expected colon.")
            val statement = statement()
            LabeledStatement(Identifier(identifier.lexeme, previous().line), statement)
        }

        match(BREAK) -> Break().also {
            expect(SEMICOLON, "Expected semicolon after 'break'.")
        }

        match(CONTINUE) -> Continue().also {
            expect(SEMICOLON, "Expected semicolon after 'continue'.")
        }

        match(WHILE) -> whileStatement()
        match(DO) -> doWhileStatement()
        match(FOR) -> forStatement()
        match(SWITCH) -> switchStatement()
        match(CASE) -> caseStatement()
        match(DEFAULT) -> defaultStatement()

        else -> ExpressionStatement(expression()).also {
            expect(SEMICOLON, "Expected semicolon after expression statement.")
        }
    }

    private fun switchStatement(): Switch {
        expect(LEFT_PAREN, "Expected ')'.")
        val expression = expression()
        expect(RIGHT_PAREN, "Expected ')'.")
        val statement = statement()
        return Switch(expression, statement)
    }

    private fun caseStatement(): ExpressionCase {
        val expression = constant()
        expect(COLON, "Expected ':'.")
        val statement = statement()
        return ExpressionCase(expression, statement)
    }

    private fun defaultStatement(): DefaultCase {
        expect(COLON, "Expected ':'.")
        val statement = if (check(RIGHT_BRACE)) NullStatement else statement()
        return DefaultCase(statement)
    }

    private fun forStatement(): For {
        expect(LEFT_PAREN, "( expected.")
        val forInit: ForInit = when {
            check(INT) -> InitDecl(declaration())
            else -> InitExpr(optionalExpression(SEMICOLON, "Expected ';' after for init expression."))
        }

        val condition = optionalExpression(SEMICOLON, "Expected ';' after condition.")
        val increment = optionalExpression(RIGHT_PAREN, "Expected ')'.")
        val body = statement()

        return For(forInit, condition, increment, body)
    }

    private fun optionalExpression(delimiter: TokenType, message: String): Expression? = when {
        !check(delimiter) -> expression()
        else -> null
    }
        .also { expect(delimiter, message) }

    private fun doWhileStatement(): DoWhile {
        val body = statement()
        expect(WHILE, "Expected 'while' in do-while loop.")
        expect(LEFT_PAREN, "( expected.")
        val condition = expression()
        expect(RIGHT_PAREN, ") expected.")
        expect(SEMICOLON, "; expected.")
        return DoWhile(condition, body)
    }

    private fun whileStatement(): While {
        expect(LEFT_PAREN, "( expected.")
        val condition = expression()
        expect(RIGHT_PAREN, ") expected.")
        val body = statement()
        return While(condition, body)
    }

    private fun ifStatement(): Statement {
        expect(LEFT_PAREN, "( expected.")
        val condition = expression()
        expect(RIGHT_PAREN, ") expected.")
        val thenBlock = statement()
        val elseBlock = if (match(ELSE)) statement() else null
        return If(condition, thenBlock, elseBlock)
    }

    private fun block(): Block {
        expect(LEFT_BRACE, "{ expected.")
        val statements = mutableListOf<BlockItem>()
        while (!check(RIGHT_BRACE)) {
            statements += blockItem()
        }
        expect(RIGHT_BRACE, "} expected.")
        return statements
    }

    private fun declaration(): Declaration = when {
        match(INT) -> {
            val identifier = expect(IDENTIFIER, "Variable name expected.").lexeme
            if (match(EQUAL)) {
                Declaration(Identifier(identifier, previous().line), expression())
            } else {
                Declaration(Identifier(identifier, previous().line), null)
            }
        }

        else -> throw error(previous(), "Unexpected declaration.")
    }.also {
        expect(SEMICOLON, "Expected semicolon.")
    }

    private fun constant(): Constant = when {
        match(CONSTANT) -> Constant(previous().literal as Int)
        else -> throw error(
            previous(),
            "Expected constant expression."
        )
    }

    private fun primary(): Expression = when {
        match(LEFT_PAREN) -> expression().also {
            expect(RIGHT_PAREN, "Expected closing ')' after expression.")
        }

        match(IDENTIFIER) -> Var(Identifier(previous().lexeme, previous().line))
        check(CONSTANT) -> constant()
        else -> throw error(
            previous(),
            "Unexpected expression ${if (peek().lexeme.isNotBlank()) "'${peek().lexeme}'" else ""}."
        )
    }

    private fun prefix(): Expression = when {
        match(INCREMENT) -> UnaryExpr(PrefixIncrement, primary())
        match(DECREMENT) -> UnaryExpr(PrefixDecrement, primary())
        else -> unary()
    }

    private fun unary(): Expression = when {
        match(MINUS) -> UnaryExpr(Negate, prefix())
        match(TILDE) -> UnaryExpr(Complement, prefix())
        match(EXCLAMATION) -> UnaryExpr(Not, prefix())
        else -> postfix()
    }

    private fun postfix(): Expression {
        var expr = primary()

        while (match(INCREMENT, DECREMENT)) {
            val op = previous().type
            expr = UnaryExpr(if (op == INCREMENT) PostfixIncrement else PostfixDecrement, expr)
        }

        return expr
    }

    private fun expression(minPrecedence: Int = 0): Expression {
        fun precedence(op: Token): Int = when (op.type) {
            EQUAL, PLUS_EQUAL, MINUS_EQUAL, ASTERISK_EQUAL, SLASH_EQUAL, PERCENT_EQUAL,
            AMPERSAND_EQUAL, PIPE_EQUAL, HAT_EQUAL, DOUBLE_LESS_EQUAL, DOUBLE_GREATER_EQUAL -> 1

            QUESTION -> 2
            DOUBLE_PIPE -> 5
            DOUBLE_AMPERSAND -> 10
            PIPE -> 25
            HAT -> 31
            AMPERSAND -> 35
            EXCLAMATION_EQUAL, DOUBLE_EQUAL -> 36
            LESS, GREATER, LESS_EQUAL, GREATER_EQUAL -> 38
            DOUBLE_LESS, DOUBLE_GREATER -> 40
            PLUS, MINUS -> 45
            ASTERISK, SLASH, PERCENT -> 50
            else -> unreachable("Unexpected token ${op}.")
        }

        var left = prefix()
        while (checkAny(
                QUESTION, EQUAL, PLUS_EQUAL, MINUS_EQUAL, ASTERISK_EQUAL, SLASH_EQUAL, PERCENT_EQUAL,
                AMPERSAND_EQUAL, PIPE_EQUAL, HAT_EQUAL, DOUBLE_LESS_EQUAL, DOUBLE_GREATER_EQUAL,
                PLUS, MINUS, ASTERISK, SLASH, PERCENT, AMPERSAND, PIPE, HAT, DOUBLE_LESS, DOUBLE_GREATER,
                DOUBLE_EQUAL, LESS, LESS_EQUAL, GREATER, GREATER_EQUAL, EXCLAMATION_EQUAL, DOUBLE_AMPERSAND, DOUBLE_PIPE
            ) && precedence(peek()) >= minPrecedence
        ) {
            val opToken = advance()
            when (opToken.type) {
                // Compound assignments
                PLUS_EQUAL, MINUS_EQUAL, ASTERISK_EQUAL, SLASH_EQUAL, PERCENT_EQUAL,
                AMPERSAND_EQUAL, PIPE_EQUAL, HAT_EQUAL, DOUBLE_LESS_EQUAL, DOUBLE_GREATER_EQUAL -> {
                    val right = expression(precedence(opToken))
                    val op = when (opToken.type) {
                        PLUS_EQUAL -> Add
                        MINUS_EQUAL -> Subtract
                        ASTERISK_EQUAL -> Multiply
                        PERCENT_EQUAL -> Remainder
                        SLASH_EQUAL -> Divide
                        AMPERSAND_EQUAL -> And
                        PIPE_EQUAL -> Or
                        HAT_EQUAL -> Xor
                        DOUBLE_LESS_EQUAL -> LeftShift
                        DOUBLE_GREATER_EQUAL -> RightShift
                        else -> unreachable("not a compound assignment operator")
                    }
                    left = Assignment(left, BinaryExpr(left, op, right))
                }

                QUESTION -> {
                    val middle = expression()
                    expect(COLON, "Expected ':' in ternary condition.")
                    val right = expression(precedence(opToken))
                    left = Conditional(left, middle, right)
                }

                EQUAL -> {
                    val right = expression(precedence(opToken))
                    left = Assignment(left, right)
                }

                else -> {
                    // Other binary expressions.
                    val op = when (opToken.type) {
                        PLUS -> Add
                        MINUS -> Subtract
                        ASTERISK -> Multiply
                        SLASH -> Divide
                        PERCENT -> Remainder
                        AMPERSAND -> And
                        PIPE -> Or
                        HAT -> Xor
                        DOUBLE_LESS -> LeftShift
                        DOUBLE_GREATER -> RightShift
                        LESS -> LessThan
                        LESS_EQUAL -> LessThanOrEqual
                        GREATER -> GreaterThan
                        GREATER_EQUAL -> GreaterThanOrEqual
                        DOUBLE_EQUAL -> Equal
                        EXCLAMATION_EQUAL -> NotEqual
                        DOUBLE_AMPERSAND -> LogicalAnd
                        DOUBLE_PIPE -> LogicalOr
                        else -> throw error(opToken, "Unexpected operator.")
                    }

                    val right = expression(precedence(opToken) + 1)
                    left = BinaryExpr(left, op, right)
                }
            }
        }

        return left
    }

    private fun optional(type: TokenType): Token? =
        if (check(type)) advance() else null

    private fun expect(type: TokenType, message: String): Token {
        if (check(type)) return advance()

        throw error(previous(), message)
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