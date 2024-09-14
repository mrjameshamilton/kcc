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
import eu.jameshamilton.frontend.TokenType.COMMA
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
import eu.jameshamilton.frontend.TokenType.SLASH
import eu.jameshamilton.frontend.TokenType.SLASH_EQUAL
import eu.jameshamilton.frontend.TokenType.STATIC
import eu.jameshamilton.frontend.TokenType.SWITCH
import eu.jameshamilton.frontend.TokenType.TILDE
import eu.jameshamilton.frontend.TokenType.UNSIGNED
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
import java.math.BigInteger

class Parser(private val tokens: List<Token>) {
    private var current = 0

    fun parse(): Program {
        val functions = mutableListOf<Declaration>()
        while (!isAtEnd()) {
            functions.add(declaration())
        }
        return Program(functions)
    }

    private fun blockItem(): BlockItem = when {
        checkSpecifier() -> declaration()
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
        val expression = expression()
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
            // storage classes are not allowed here, but it will be checked in the semantic phase.
            checkSpecifier() -> InitDecl(declaration() as VarDeclaration)
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

    private fun varDeclaration(type: Type, identifier: Identifier, storageClass: StorageClass): VarDeclaration = when {
        match(EQUAL) -> VarDeclaration(identifier, initializer(), type, storageClass)
        else -> VarDeclaration(identifier, type, storageClass)
    }.also {
        expect(SEMICOLON, "Expected semicolon.")
    }

    private fun initializer(): Initializer = when {
        match(LEFT_BRACE) -> {
            val initializers = mutableListOf<Initializer>()
            do {
                initializers.add(initializer())
                if (check(RIGHT_BRACE)) {
                    optional(COMMA)
                } else {
                    expect(COMMA, "Expected ','.")
                }
            } while (!match(RIGHT_BRACE))
            CompoundInit(initializers)
        }

        else -> SingleInit(expression())
    }

    private fun declaration(): Declaration {
        val (type, storageClass) = specifier()
        val declarator = declarator()
        val (identifier, declType, parameters) = processDeclarator(declarator, type)
        return if (declType is FunType) {
            val body = if (check(LEFT_BRACE)) {
                block()
            } else {
                expect(SEMICOLON, "Expected semicolon after function declaration.")
                null
            }

            return FunDeclaration(
                identifier,
                parameters,
                body,
                declType,
                storageClass
            )
        } else {
            varDeclaration(declType, identifier, storageClass)
        }
    }

    private sealed class Declarator
    private data class Ident(val identifier: Identifier) : Declarator()
    private data class PointerDeclarator(val declarator: Declarator) : Declarator()
    private data class ArrayDeclarator(val declarator: Declarator, val size: Int) : Declarator()
    private data class FunctionDeclarator(val params: List<ParamInfo>, val declarator: Declarator) : Declarator()
    private data class ParamInfo(val type: Type, val declarator: Declarator)


    private fun declarator(): Declarator = when {
        match(ASTERISK) -> PointerDeclarator(declarator())
        else -> directDeclarator()
    }

    private fun simpleDeclarator(): Declarator = when {
        match(IDENTIFIER) -> Ident(Identifier(previous().lexeme, previous().line))
        match(LEFT_PAREN) -> declarator().also {
            expect(RIGHT_PAREN, "Expected ')'.")
        }

        else -> throw error(peek(), "Error parsing simple declarator")
    }

    private fun directDeclarator(): Declarator {
        var simple = simpleDeclarator()
        if (check(LEFT_PAREN)) {
            return FunctionDeclarator(params(), simple)
        } else {
            while (match(LEFT_BRACKET)) {
                val size = arraySize()
                simple = ArrayDeclarator(simple, size)
                expect(RIGHT_BRACKET, "Expected ']'.")
            }
        }
        return simple
    }

    private fun params(): List<ParamInfo> {
        val params = mutableListOf<ParamInfo>()
        expect(LEFT_PAREN, "Expected '('.")
        if (match(VOID)) {
            // no params
        } else {
            do {
                params.add(ParamInfo(typeSpecifier(), declarator()))
            } while (match(COMMA))
        }
        expect(RIGHT_PAREN, "Expected ')'.")
        return params
    }

    private fun processDeclarator(
        declarator: Declarator,
        baseType: Type
    ): Triple<Identifier, Type, List<VarDeclaration>> = when (declarator) {
        is FunctionDeclarator -> when (declarator.declarator) {
            is Ident -> {
                val parameters = declarator.params.associate {
                    val (name, type, _) = processDeclarator(it.declarator, it.type)
                    if (type is FunType) {
                        throw error(previous(), "Function pointers in parameters aren't supported.")
                    }
                    name to type
                }
                val derivedType = FunType(parameters.values.toList(), baseType)
                Triple(
                    declarator.declarator.identifier,
                    derivedType,
                    parameters.map { VarDeclaration(it.key, null, it.value) })
            }

            is FunctionDeclarator, is PointerDeclarator -> throw error(
                previous(),
                "Can't apply additional type derivations to a function type"
            )

            is ArrayDeclarator -> TODO()
        }

        is Ident -> Triple(declarator.identifier, baseType, emptyList())
        is PointerDeclarator -> processDeclarator(declarator.declarator, PointerType(baseType))
        is ArrayDeclarator -> processDeclarator(declarator.declarator, ArrayType(baseType, declarator.size))
    }

    private fun processAbstractDeclarator(declarator: AbstractDeclarator, baseType: Type): Type = when (declarator) {
        is AbstractBase -> baseType
        is AbstractPointer -> processAbstractDeclarator(declarator.abstractDeclarator, PointerType(baseType))
        is AbstractArray -> processAbstractDeclarator(
            declarator.abstractDeclarator,
            ArrayType(baseType, declarator.size)
        )
    }

    private sealed class AbstractDeclarator
    private data class AbstractPointer(val abstractDeclarator: AbstractDeclarator) : AbstractDeclarator()
    private data class AbstractArray(val abstractDeclarator: AbstractDeclarator, val size: Int) : AbstractDeclarator()
    private data object AbstractBase : AbstractDeclarator()

    private fun abstractDeclarator(): AbstractDeclarator = when {
        match(ASTERISK) -> AbstractPointer(abstractDeclarator())
        checkAny(LEFT_PAREN, LEFT_BRACKET) -> directAbstractDeclarator()
        else -> AbstractBase
    }

    private fun directAbstractDeclarator(): AbstractDeclarator {
        var abstractDeclarator = when {
            match(LEFT_PAREN) -> {
                abstractDeclarator().also {
                    expect(RIGHT_PAREN, "Expected ')'.")
                }
            }

            else -> AbstractBase
        }

        while (match(LEFT_BRACKET)) {
            val size = arraySize()
            abstractDeclarator = AbstractArray(abstractDeclarator, size)
            expect(RIGHT_BRACKET, "Expected ']'.")
        }
        return abstractDeclarator
    }

    private fun arraySize(): Int = when {
        match(CONSTANT_INT) -> (previous().literal as BigInteger).toInt()
        match(CONSTANT_LONG, CONSTANT_UINT, CONSTANT_ULONG) -> (previous().literal as BigInteger).toInt().also {
            System.err.println("Warning: array size truncated to integer.")
        }

        else -> throw error(peek(), "Invalid array size '${previous().lexeme}'.")
    }

    private fun checkSpecifier() = checkType() || checkStorageClass()
    private fun checkStorageClass() = check(EXTERN) || check(STATIC)
    private fun checkType() = check(INT) || check(LONG) || check(DOUBLE) || check(SIGNED) || check(UNSIGNED)

    private fun typeSpecifier() = specifier(allowStorageClass = false).first
    private fun specifier(allowStorageClass: Boolean = true): Pair<Type, StorageClass> {
        val typeTokens = mutableListOf<TokenType>()
        val storageClasses = mutableListOf<StorageClass>()

        do {
            when {
                checkType() -> typeTokens.add(advance().type)
                checkStorageClass() -> if (allowStorageClass) {
                    storageClasses.add(storageClass())
                } else {
                    throw error(peek(), "Storage class specifiers not allowed here.")
                }

                else -> throw error(peek(), "Unexpected specifier '${peek().lexeme}'.")
            }
        } while (checkType() || checkStorageClass())

        val type = type(typeTokens)

        val storageClass = when {
            storageClasses.size == 1 -> storageClasses.single()
            storageClasses.isNotEmpty() -> {
                throw error(previous(), "Expected only 1 storage class, found ${storageClasses.joinToString(", ")}.")
            }

            else -> StorageClass.NONE
        }

        return Pair(type, storageClass)
    }

    private fun storageClass(): StorageClass = when {
        match(STATIC) -> StorageClass.STATIC
        match(EXTERN) -> StorageClass.EXTERN
        else -> throw error(previous(), "Unexpected storage class '${peek().type}'.")
    }

    private fun type(types: List<TokenType>): Type = when {
        types == listOf(DOUBLE) -> DoubleType

        types.contains(DOUBLE) -> {
            throw error(previous(), "`double` cannot be combined with other types '${types.joinToString(", ")}'.")
        }

        types.isEmpty() -> {
            throw error(previous(), "Expected at least one type specifier.")
        }

        types.contains(SIGNED) && types.contains(UNSIGNED) -> {
            throw error(previous(), "Invalid type specifier '${types.joinToString(", ")}'")
        }

        types.distinct().size != types.count() -> {
            throw error(previous(), "Duplicate type specifiers '${types.joinToString(", ")}'.")
        }

        types.containsAll(setOf(UNSIGNED, LONG)) -> ULongType
        types.contains(UNSIGNED) -> UIntType
        types.contains(LONG) -> LongType

        // Default type is int.
        else -> IntType
    }

    private fun primary(): Expression = when {
        match(LEFT_PAREN) -> when {
            checkType() -> {
                val type = typeSpecifier()
                val declarator = abstractDeclarator()
                expect(RIGHT_PAREN, "Expected ')' after expression.")
                Cast(processAbstractDeclarator(declarator, type), unary())
            }

            else -> expression().also {
                expect(RIGHT_PAREN, "Expected closing ')' after expression.")
            }
        }

        match(ASTERISK) -> Dereference(primary())

        match(IDENTIFIER) -> {
            val identifier = Identifier(previous().lexeme, previous().line)
            if (match(LEFT_PAREN)) {
                val arguments = mutableListOf<Expression>()
                if (!check(RIGHT_PAREN)) {
                    do {
                        arguments += expression()
                    } while (match(COMMA))
                }
                expect(RIGHT_PAREN, "Expected ')' after function call.")
                FunctionCall(identifier, arguments)
            } else {
                Var(identifier)
            }
        }

        match(CONSTANT_INT, CONSTANT_UINT) -> {
            val literal = previous().literal as BigInteger
            val unsigned = previous().type == CONSTANT_UINT

            if (unsigned) {
                when {
                    literal >= BigInteger.ZERO && literal <= BigInteger.valueOf(4294967295) -> {
                        Constant(literal.toLong().toUInt())
                    }

                    literal >= BigInteger.ZERO && literal <= BigInteger.TWO.pow(64) - BigInteger.ONE -> {
                        Constant(literal.toLong().toULong())
                    }

                    else -> throw error(previous(), "unsigned integer out of supported range.")
                }
            } else {
                when {
                    literal >= Int.MIN_VALUE.toBigInteger() && literal <= Int.MAX_VALUE.toBigInteger() -> {
                        Constant(literal.toInt())
                    }

                    literal >= Long.MIN_VALUE.toBigInteger() && literal <= Long.MAX_VALUE.toBigInteger() -> {
                        Constant(literal.toLong())
                    }

                    else -> throw error(previous(), "Integer out of supported range.")
                }
            }
        }

        match(CONSTANT_LONG, CONSTANT_ULONG) -> {

            val literal = previous().literal as BigInteger
            val unsigned = previous().type == CONSTANT_ULONG
            if (unsigned) {
                if (literal >= BigInteger.ZERO && literal <= (BigInteger.TWO.pow(64) - BigInteger.ONE)) {
                    Constant(literal.toLong().toULong())
                } else {
                    throw error(previous(), "Unsigned long constant out of supported range.")
                }
            } else {
                if (literal >= Long.MIN_VALUE.toBigInteger() && literal <= Long.MAX_VALUE.toBigInteger()) {
                    Constant(literal.toLong())
                } else {
                    throw error(previous(), "Long constant out of supported range.")
                }
            }
        }

        match(CONSTANT_DOUBLE) -> Constant(previous().literal as Double)

        else -> throw error(
            previous(),
            "Unexpected expression ${if (peek().lexeme.isNotBlank()) "'${peek().lexeme}'" else ""}."
        )
    }

    private fun unary(): Expression = when {
        match(INCREMENT) -> UnaryExpr(PrefixIncrement, unary())
        match(DECREMENT) -> UnaryExpr(PrefixDecrement, unary())
        match(MINUS) -> when (val expression = unary()) {
            is Constant -> when (expression.value) {
                is Int -> Constant(-expression.value)
                is Long -> Constant(-expression.value)
                is Double -> Constant(-expression.value)
                else -> UnaryExpr(Negate, expression)
            }

            else -> UnaryExpr(Negate, expression)
        }

        match(TILDE) -> UnaryExpr(Complement, unary())
        match(EXCLAMATION) -> UnaryExpr(Not, unary())
        match(ASTERISK) -> Dereference(unary())
        match(AMPERSAND) -> AddrOf(unary())
        else -> postfix()
    }

    private fun postfix(): Expression {
        var expr = primary()

        while (match(LEFT_BRACKET, INCREMENT, DECREMENT)) when (previous().type) {
            INCREMENT -> expr = UnaryExpr(PostfixIncrement, expr)
            DECREMENT -> expr = UnaryExpr(PostfixDecrement, expr)
            LEFT_BRACKET -> expr = Subscript(expr, expression()).also {
                expect(RIGHT_BRACKET, "Expected closing ']' after expression.")
            }

            else -> {}
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

        var left = unary()
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
                    left = Assignment(left, BinaryExpr(left, op, right), compound = true)
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