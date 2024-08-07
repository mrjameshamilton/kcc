package eu.jameshamilton.frontend

enum class TokenType {
    LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE, LEFT_BRACKET, RIGHT_BRACKET,

    SEMICOLON,

    TILDE, MINUS, DECREMENT, INCREMENT, PLUS, ASTERISK, SLASH, PERCENT,

    IDENTIFIER, STRING, INT, VOID, RETURN,

    CONSTANT,

    EOF
}

data class Token(val type: TokenType, val lexeme: String, val literal: Any? = null, val line: Int = -1)
