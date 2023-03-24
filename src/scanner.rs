use crate::token::{Lexeme, Token, TokenType};
use peekmore::{PeekMore, PeekMoreIterator};
use std::str::Chars;

#[derive(Debug)]
pub struct Scanner<'a> {
    source: &'a str,
    iter: PeekMoreIterator<Chars<'a>>,
    tokens: Vec<Token<'a>>,
    start: usize,
    current: usize,
    line: u32,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source: source,
            iter: source.chars().peekmore(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> &Vec<Token<'a>> {
        loop {
            self.whitespace();
            self.start = self.current;
            match self.advance() {
                Some('(') => self.add_token(TokenType::LeftParen),
                Some(')') => self.add_token(TokenType::RightParen),
                Some('{') => self.add_token(TokenType::LeftBrace),
                Some('}') => self.add_token(TokenType::RightBrace),
                Some(',') => self.add_token(TokenType::Comma),
                Some('.') => self.add_token(TokenType::Dot),
                Some('-') => self.add_token(TokenType::Minus),
                Some('+') => self.add_token(TokenType::Plus),
                Some(';') => self.add_token(TokenType::Semicolon),
                Some('*') => self.add_token(TokenType::Star),
                Some('/') => self.add_token(TokenType::Slash),

                Some('!') => self.add_matches_token(TokenType::Bang, '=', TokenType::BangEqual),
                Some('=') => self.add_matches_token(TokenType::Equal, '=', TokenType::EqualEqual),
                Some('<') => self.add_matches_token(TokenType::Less, '=', TokenType::LessEqual),
                Some('>') => {
                    self.add_matches_token(TokenType::Greater, '=', TokenType::GreaterEqual)
                }

                Some('"') => self.string(),
                Some('0'..='9') => self.number(),
                Some('a'..='z') | Some('A'..='Z') | Some('_') => self.identifier(),

                Some(character) => {
                    self.report(format!("Unexpect character [{}]", character).as_str())
                }

                None => break,
            }
        }
        &self.tokens
    }

    fn report(&self, message: &str) {
        println!("[line {}] Error: {}", self.line, message);
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.tokens.push(Token {
            lexeme: {
                if token_type == TokenType::Number {
                    Lexeme::Number(self.source[self.start..self.current].parse().unwrap())
                } else {
                    Lexeme::String(&self.source[self.start..self.current])
                }
            },
            token_type: token_type,
            line: self.line,
        });
    }

    fn add_matches_token(
        &mut self,
        unmatches_token_type: TokenType,
        matches_char: char,
        matches_token_type: TokenType,
    ) {
        let token_type = if self.peek() == Some(&matches_char) {
            self.advance();
            matches_token_type
        } else {
            unmatches_token_type
        };
        self.add_token(token_type);
    }

    fn whitespace(&mut self) {
        loop {
            match self.peek() {
                Some(' ') | Some('\r') | Some('\t') => {
                    self.advance();
                }
                Some('\n') => {
                    self.advance();
                    self.line += 1;
                }
                Some('/') => {
                    if self.peek_next() == Some(&'/') {
                        loop {
                            match self.peek() {
                                Some('\n') | None => {
                                    break;
                                }
                                _ => {
                                    self.advance();
                                }
                            }
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
    }

    fn string(&mut self) -> () {
        self.start += 1;
        loop {
            match self.peek() {
                Some('"') => {
                    self.add_token(TokenType::String);
                    self.advance();
                    break;
                }
                Some('\n') => {
                    self.advance();
                    self.line += 1;
                }
                None => {
                    self.report("Unterminated string.");
                    break;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    // Panic: Values exceeding f64's range will result in a panic.
    fn number(&mut self) -> () {
        loop {
            match self.peek() {
                Some('0'..='9') => {
                    self.advance();
                }
                _ => break,
            }
        }

        if self.peek() == Some(&'.') && self.peek_next().unwrap().is_digit(10) {
            self.advance();
            loop {
                match self.peek() {
                    Some('0'..='9') => {
                        self.advance();
                    }
                    _ => break,
                }
            }
        }

        self.add_token(TokenType::Number);
    }

    fn identifier(&mut self) -> () {
        loop {
            match self.peek() {
                Some('a'..='z') | Some('A'..='Z') | Some('0'..='9') | Some('_') => {
                    self.advance();
                }
                _ => break,
            }
        }

        let lexeme = &self.source[self.start..self.current];
        let token_type = match lexeme {
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fun" => TokenType::Fun,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            _ => TokenType::Identifier,
        };
        self.add_token(token_type);
    }

    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        self.iter.next()
    }

    fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }

    fn peek_next(&mut self) -> Option<&char> {
        self.iter.peek_nth(1)
    }
}

#[cfg(test)]
mod tests {
    use crate::scanner::Scanner;
    use crate::token::{Lexeme, Token, TokenType};
    use peekmore::PeekMore;

    // a macro for better writing tests
    // maybe have problems
    // expand example:
    // taken a value of [tokens, (Star, "*", 1), ...]
    // will expand to =>
    // let mut pos:usize = 0;
    // assert_token(
    //     tokens.get(pos).unwrap(),
    //     TokenType::BangEqual,
    //     Lexeme::String("!="),
    //     1,
    // );
    // pos = pos + 1;
    // ...;
    // one can expand macro by call `Inline macro' code action in lsp
    // TODO: why warn `value assigned to pos is never read'
    macro_rules! assert_tokens {
        ( $tokens:expr, $( ($type:tt, $lexeme:literal, $line:literal) ),* ) => (
            let mut pos:usize = 0;
            $(
assert_token(
    $tokens.get(pos).unwrap(),
    TokenType::$type,
    // if Lexeme:String
    Lexeme::String($lexeme),
    $line,
);
pos = pos + 1;
            )*
        );
    }
    
    // this macro exists becasue i dont know how to let rust macro expand accordingly to the type of lexeme.
    // for example in lisp. the macro can exapnd differently if (stringp $lexeme) return true
    // since we have a special macro for number, we can omit the Number field in param
    macro_rules! assert_tokens_number {
        ( $tokens:expr, $( ($lexeme:literal, $line:literal) ),* ) => (
            let mut pos:usize = 0;
            $(
assert_token(
    $tokens.get(pos).unwrap(),
    TokenType::Number,
    Lexeme::Number($lexeme),
    $line,
);
pos = pos + 1;
            )*
        );
    }
    
    #[test]
    fn iterator() {
        let mut iter = "abcde".chars().peekmore();

        assert_eq!(iter.peek(), Some(&'a'));
        assert_eq!(iter.peek_nth(1), Some(&'b'));
        assert_eq!(iter.peek_nth(2), Some(&'c'));
        assert_eq!(iter.peek_nth(3), Some(&'d'));
    }

    #[test]
    fn single_character() {
        let mut scanner = Scanner::new("* + - ( } ) { , . ;");
        let tokens = scanner.scan_tokens();
        
        assert_tokens![tokens,
            (Star, "*", 1),
            (Plus, "+", 1),
            (Minus, "-", 1),
            (LeftParen, "(", 1),
            (RightBrace, "}", 1),
            (RightParen, ")", 1),
            (LeftBrace, "{", 1),
            (Comma, ",", 1),
            (Dot, ".", 1),
            (Semicolon, ";", 1)
        ];
    }

    #[test]
    fn two_character() {
        let mut scanner = Scanner::new("!= == <= >=");
        let tokens = scanner.scan_tokens();
        assert_tokens!(tokens,
            (BangEqual, "!=", 1),
            (EqualEqual, "==", 1),
            (LessEqual, "<=", 1),
            (GreaterEqual, ">=", 1)
        );
    }

    #[test]
    fn whitespace() {
        let mut scanner = Scanner::new(" + \t - \r * \n = // test hahahhaha \n > <");
        let tokens = scanner.scan_tokens();
        assert_tokens!(tokens,
            (Plus, "+", 1),
            (Minus, "-", 1),
            (Star, "*", 1),
            (Equal, "=", 2),
            (Greater, ">", 3),
            (Less, "<", 3)
        );
    }

    #[test]
    fn slash() {
        let mut scanner = Scanner::new("// test hahahhaha \n + - * / // + -");
        let tokens = scanner.scan_tokens();
        assert_tokens!(tokens,
            (Plus, "+", 2),
            (Minus, "-", 2),
            (Star, "*", 2),
            (Slash, "/", 2)
        );
    }

    #[test]
    fn string() {
        let mut scanner = Scanner::new(r##" "test" "test2" "test3" "##);
        let tokens = scanner.scan_tokens();
        assert_tokens!(tokens,
            (String, "test", 1),
            (String, "test2", 1),
            (String, "test3", 1)
        );
    }

    #[test]
    fn number() {
        let mut scanner = Scanner::new("123 123.123 123.7890123 123.7890123456789 4242489748472");
        let tokens = scanner.scan_tokens();
        assert_tokens_number!(tokens,
            (123.0, 1),
            (123.123, 1),
            (123.7890123, 1),
            (123.7890123456789, 1),
            (4242489748472.0, 1)
        );
    }

    #[test]
    fn identifier() {
        let mut scanner = Scanner::new("test test2 test3");
        let tokens = scanner.scan_tokens();
        assert_tokens!(tokens,
            (Identifier, "test", 1),
            (Identifier, "test2", 1),
            (Identifier, "test3", 1)
        );
    }

    #[test]
    fn keyword() {
        let mut scanner = Scanner::new(
            "and class else false for fun if nil or print return super this true var while",
        );
        let tokens = scanner.scan_tokens();
        assert_tokens!(tokens,
            (And, "and", 1),
            (Class, "class", 1),
            (Else, "else", 1),
            (False, "false", 1),
            (For, "for", 1),
            (Fun, "fun", 1),
            (If, "if", 1),
            (Nil, "nil", 1),
            (Or, "or", 1),
            (Print, "print", 1),
            (Return, "return", 1),
            (Super, "super", 1),
            (This, "this", 1),
            (True, "true", 1),
            (Var, "var", 1),
            (While, "while", 1)
        );
    }

    fn assert_token(token: &Token, token_type: TokenType, lexeme: Lexeme, line: u32) {
        assert_eq!(token.token_type, token_type);
        assert_eq!(token.lexeme, lexeme);
        assert_eq!(token.line, line);
    }
}
