pub enum TokenType {
    // Single-character tokens
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    // One or two character tokens
    Bang, BangEqual,
    Equal, EqulaEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    // Literals
    Identifier, String, Number,
    
    // Keywords
    And, Class, Else, False,For, Fun, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,
    
    Eof
}