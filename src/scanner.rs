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

    pub fn scan_tokens(&mut self) {
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

                Some('!') => self.add_matches_token(TokenType::Bang, '=', TokenType::BangEqual),
                Some('=') => self.add_matches_token(TokenType::Equal, '=', TokenType::EqulaEqual),
                Some('<') => self.add_matches_token(TokenType::Less, '=', TokenType::LessEqual),
                Some('>') => {
                    self.add_matches_token(TokenType::Greater, '=', TokenType::GreaterEqual)
                }

                Some(character) => {
                    self.report(format!("Unexpect character [{}]", character).as_str())
                }

                None => break,
            }
        }
    }

    fn report(&self, message: &str) {
        println!("[line {}] Error: {}", self.line, message);
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.tokens.push(Token {
            token_type: token_type,
            line: self.line,
            lexeme: Lexeme::String(&self.source[self.start..self.current]),
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
                        self.advance();
                        self.advance();
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
                    }
                }
                _ => break,
            }
        }
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
    use crate::token::{Lexeme, TokenType};
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
    fn signle_charater() {
        let mut scanner = Scanner::new("* + - ( } ) { , . ;");
        scanner.scan_tokens();
        assert_eq!(scanner.tokens.get(0).unwrap().token_type, TokenType::Star);
        assert_eq!(scanner.tokens.get(0).unwrap().lexeme, Lexeme::String("*"));
        assert_eq!(scanner.tokens.get(1).unwrap().token_type, TokenType::Plus);
        assert_eq!(scanner.tokens.get(1).unwrap().lexeme, Lexeme::String("+"));
        assert_eq!(scanner.tokens.get(2).unwrap().token_type, TokenType::Minus);
        assert_eq!(
            scanner.tokens.get(3).unwrap().token_type,
            TokenType::LeftParen
        );
        assert_eq!(
            scanner.tokens.get(4).unwrap().token_type,
            TokenType::RightBrace
        );
        assert_eq!(
            scanner.tokens.get(5).unwrap().token_type,
            TokenType::RightParen
        );
        assert_eq!(
            scanner.tokens.get(6).unwrap().token_type,
            TokenType::LeftBrace
        );
        assert_eq!(scanner.tokens.get(7).unwrap().token_type, TokenType::Comma);
        assert_eq!(scanner.tokens.get(8).unwrap().token_type, TokenType::Dot);
        assert_eq!(
            scanner.tokens.get(9).unwrap().token_type,
            TokenType::Semicolon
        );
    }

    #[test]
    fn two_character() {
        let mut scanner = Scanner::new("*!=!+ - ( } ) { , . ;");
        scanner.scan_tokens();
        assert_eq!(scanner.tokens.get(0).unwrap().token_type, TokenType::Star);
        assert_eq!(scanner.tokens.get(0).unwrap().lexeme, Lexeme::String("*"));
        assert_eq!(
            scanner.tokens.get(1).unwrap().token_type,
            TokenType::BangEqual
        );
        assert_eq!(scanner.tokens.get(1).unwrap().lexeme, Lexeme::String("!="));
        assert_eq!(scanner.tokens.get(2).unwrap().token_type, TokenType::Bang);
        assert_eq!(scanner.tokens.get(2).unwrap().lexeme, Lexeme::String("!"));
    }

    #[test]
    fn whitespace() {
        let mut scanner = Scanner::new("    \n    * //     \n !=   \n!+ - ( } ) { , . ;");
        scanner.scan_tokens();
        assert_eq!(scanner.tokens.get(0).unwrap().token_type, TokenType::Star);
        assert_eq!(scanner.tokens.get(0).unwrap().line, 2);
        assert_eq!(scanner.tokens.get(0).unwrap().lexeme, Lexeme::String("*"));
        assert_eq!(
            scanner.tokens.get(1).unwrap().token_type,
            TokenType::BangEqual
        );
        assert_eq!(scanner.tokens.get(1).unwrap().lexeme, Lexeme::String("!="));
        assert_eq!(scanner.tokens.get(1).unwrap().line, 3);
        assert_eq!(scanner.tokens.get(2).unwrap().token_type, TokenType::Bang);
        assert_eq!(scanner.tokens.get(2).unwrap().line, 4);
        assert_eq!(scanner.tokens.get(3).unwrap().token_type, TokenType::Plus);
        assert_eq!(scanner.tokens.get(3).unwrap().line, 4);
    }
}
