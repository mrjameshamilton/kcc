package eu.jameshamilton.frontend

enum class TokenType {
    LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE, LEFT_BRACKET, RIGHT_BRACKET,

    SEMICOLON, COMMA,

    TILDE, MINUS, DECREMENT, INCREMENT, PLUS, ASTERISK, SLASH, PERCENT,

    AMPERSAND, PIPE, HAT, DOUBLE_LESS, LESS, LESS_EQUAL, DOUBLE_GREATER, GREATER, GREATER_EQUAL,

    EXCLAMATION, DOUBLE_AMPERSAND, DOUBLE_PIPE, EQUAL, DOUBLE_EQUAL, EXCLAMATION_EQUAL,

    // Compound assignments.
    PLUS_EQUAL, MINUS_EQUAL, ASTERISK_EQUAL, PERCENT_EQUAL, SLASH_EQUAL,
    AMPERSAND_EQUAL, PIPE_EQUAL, HAT_EQUAL, DOUBLE_LESS_EQUAL, DOUBLE_GREATER_EQUAL,

    IDENTIFIER, STRING, INT, LONG, VOID, RETURN,

    EXTERN, STATIC,

    CONSTANT_INT, CONSTANT_LONG, CONSTANT_UINT, CONSTANT_ULONG,

    SIGNED, UNSIGNED,

    // Conditionals.
    IF, ELSE, QUESTION, COLON,

    GOTO, DO, WHILE, FOR, BREAK, CONTINUE,

    SWITCH, CASE, DEFAULT,

    EOF
}

data class Token(val type: TokenType, val lexeme: String, val literal: Any? = null, val line: Int = -1)
