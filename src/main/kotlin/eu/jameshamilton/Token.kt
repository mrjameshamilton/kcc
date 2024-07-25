package eu.jameshamilton

enum class TokenType {
    LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE, LEFT_BRACKET, RIGHT_BRACKET,

    SEMICOLON,

    IDENTIFIER, STRING, INT, VOID, RETURN,

    CONSTANT,

    EOF
}

data class Token(val type: TokenType, val lexeme: String, val literal: Any? = null, val line: Int = -1)
