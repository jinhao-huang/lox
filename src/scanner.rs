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
        assert_token(
            tokens.get(0).unwrap(),
            TokenType::Star,
            Lexeme::String("*"),
            1,
        );
        assert_token(
            tokens.get(1).unwrap(),
            TokenType::Plus,
            Lexeme::String("+"),
            1,
        );
        assert_token(
            tokens.get(2).unwrap(),
            TokenType::Minus,
            Lexeme::String("-"),
            1,
        );
        assert_token(
            tokens.get(3).unwrap(),
            TokenType::LeftParen,
            Lexeme::String("("),
            1,
        );
        assert_token(
            tokens.get(4).unwrap(),
            TokenType::RightBrace,
            Lexeme::String("}"),
            1,
        );
        assert_token(
            tokens.get(5).unwrap(),
            TokenType::RightParen,
            Lexeme::String(")"),
            1,
        );
        assert_token(
            tokens.get(6).unwrap(),
            TokenType::LeftBrace,
            Lexeme::String("{"),
            1,
        );
        assert_token(
            tokens.get(7).unwrap(),
            TokenType::Comma,
            Lexeme::String(","),
            1,
        );
        assert_token(
            tokens.get(8).unwrap(),
            TokenType::Dot,
            Lexeme::String("."),
            1,
        );
        assert_token(
            tokens.get(9).unwrap(),
            TokenType::Semicolon,
            Lexeme::String(";"),
            1,
        );
    }

    #[test]
    fn two_character() {
        let mut scanner = Scanner::new("!= == <= >=");
        let tokens = scanner.scan_tokens();
        assert_token(
            tokens.get(0).unwrap(),
            TokenType::BangEqual,
            Lexeme::String("!="),
            1,
        );
        assert_token(
            tokens.get(1).unwrap(),
            TokenType::EqualEqual,
            Lexeme::String("=="),
            1,
        );
        assert_token(
            tokens.get(2).unwrap(),
            TokenType::LessEqual,
            Lexeme::String("<="),
            1,
        );
        assert_token(
            tokens.get(3).unwrap(),
            TokenType::GreaterEqual,
            Lexeme::String(">="),
            1,
        );
    }

    #[test]
    fn whitespace() {
        let mut scanner = Scanner::new(" + \t - \r * \n = // test hahahhaha \n > <");
        let tokens = scanner.scan_tokens();
        assert_token(
            tokens.get(0).unwrap(),
            TokenType::Plus,
            Lexeme::String("+"),
            1,
        );
        assert_token(
            tokens.get(1).unwrap(),
            TokenType::Minus,
            Lexeme::String("-"),
            1,
        );
        assert_token(
            tokens.get(2).unwrap(),
            TokenType::Star,
            Lexeme::String("*"),
            1,
        );
        assert_token(
            tokens.get(3).unwrap(),
            TokenType::Equal,
            Lexeme::String("="),
            2,
        );
        assert_token(
            tokens.get(4).unwrap(),
            TokenType::Greater,
            Lexeme::String(">"),
            3,
        );
        assert_token(
            tokens.get(5).unwrap(),
            TokenType::Less,
            Lexeme::String("<"),
            3,
        );
    }

    #[test]
    fn slash() {
        let mut scanner = Scanner::new("// test hahahhaha \n + - * / // + -");
        let tokens = scanner.scan_tokens();
        assert_token(
            tokens.get(0).unwrap(),
            TokenType::Plus,
            Lexeme::String("+"),
            2,
        );
        assert_token(
            tokens.get(1).unwrap(),
            TokenType::Minus,
            Lexeme::String("-"),
            2,
        );
        assert_token(
            tokens.get(2).unwrap(),
            TokenType::Star,
            Lexeme::String("*"),
            2,
        );
        assert_token(
            tokens.get(3).unwrap(),
            TokenType::Slash,
            Lexeme::String("/"),
            2,
        );
        assert!(tokens.get(4).is_none());
    }

    #[test]
    fn string() {
        let mut scanner = Scanner::new(r##" "test" "test2" "test3" "##);
        let tokens = scanner.scan_tokens();
        assert_token(
            tokens.get(0).unwrap(),
            TokenType::String,
            Lexeme::String("test"),
            1,
        );
        assert_token(
            tokens.get(1).unwrap(),
            TokenType::String,
            Lexeme::String("test2"),
            1,
        );
        assert_token(
            tokens.get(2).unwrap(),
            TokenType::String,
            Lexeme::String("test3"),
            1,
        );
    }

    #[test]
    fn number() {
        let mut scanner = Scanner::new("123 123.123 123.7890123 123.7890123456789 4242489748472");
        let tokens = scanner.scan_tokens();
        assert_token(
            tokens.get(0).unwrap(),
            TokenType::Number,
            Lexeme::Number(123.0),
            1,
        );
        assert_token(
            tokens.get(1).unwrap(),
            TokenType::Number,
            Lexeme::Number(123.123),
            1,
        );
        assert_token(
            tokens.get(2).unwrap(),
            TokenType::Number,
            Lexeme::Number(123.7890123),
            1,
        );
        assert_token(
            tokens.get(3).unwrap(),
            TokenType::Number,
            Lexeme::Number(123.7890123456789),
            1,
        );
        assert_token(
            tokens.get(4).unwrap(),
            TokenType::Number,
            Lexeme::Number(4242489748472.0),
            1,
        );
    }

    #[test]
    fn identifier() {
        let mut scanner = Scanner::new("test test2 test3");
        let tokens = scanner.scan_tokens();
        assert_token(
            tokens.get(0).unwrap(),
            TokenType::Identifier,
            Lexeme::String("test"),
            1,
        );
        assert_token(
            tokens.get(1).unwrap(),
            TokenType::Identifier,
            Lexeme::String("test2"),
            1,
        );
        assert_token(
            tokens.get(2).unwrap(),
            TokenType::Identifier,
            Lexeme::String("test3"),
            1,
        );
    }

    #[test]
    fn keyword() {
        let mut scanner = Scanner::new(
            "and class else false for fun if nil or print return super this true var while",
        );
        let tokens = scanner.scan_tokens();
        assert_token(
            tokens.get(0).unwrap(),
            TokenType::And,
            Lexeme::String("and"),
            1,
        );
        assert_token(
            tokens.get(1).unwrap(),
            TokenType::Class,
            Lexeme::String("class"),
            1,
        );
        assert_token(
            tokens.get(2).unwrap(),
            TokenType::Else,
            Lexeme::String("else"),
            1,
        );
        assert_token(
            tokens.get(3).unwrap(),
            TokenType::False,
            Lexeme::String("false"),
            1,
        );
        assert_token(
            tokens.get(4).unwrap(),
            TokenType::For,
            Lexeme::String("for"),
            1,
        );
        assert_token(
            tokens.get(5).unwrap(),
            TokenType::Fun,
            Lexeme::String("fun"),
            1,
        );
        assert_token(
            tokens.get(6).unwrap(),
            TokenType::If,
            Lexeme::String("if"),
            1,
        );
        assert_token(
            tokens.get(7).unwrap(),
            TokenType::Nil,
            Lexeme::String("nil"),
            1,
        );
        assert_token(
            tokens.get(8).unwrap(),
            TokenType::Or,
            Lexeme::String("or"),
            1,
        );
        assert_token(
            tokens.get(9).unwrap(),
            TokenType::Print,
            Lexeme::String("print"),
            1,
        );
        assert_token(
            tokens.get(10).unwrap(),
            TokenType::Return,
            Lexeme::String("return"),
            1,
        );
        assert_token(
            tokens.get(11).unwrap(),
            TokenType::Super,
            Lexeme::String("super"),
            1,
        );
        assert_token(
            tokens.get(12).unwrap(),
            TokenType::This,
            Lexeme::String("this"),
            1,
        );
        assert_token(
            tokens.get(13).unwrap(),
            TokenType::True,
            Lexeme::String("true"),
            1,
        );
        assert_token(
            tokens.get(14).unwrap(),
            TokenType::Var,
            Lexeme::String("var"),
            1,
        );
        assert_token(
            tokens.get(15).unwrap(),
            TokenType::While,
            Lexeme::String("while"),
            1,
        );
    }

    fn assert_token(token: &Token, token_type: TokenType, lexeme: Lexeme, line: u32) {
        assert_eq!(token.token_type, token_type);
        assert_eq!(token.lexeme, lexeme);
        assert_eq!(token.line, line);
    }
}
