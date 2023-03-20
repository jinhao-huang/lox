use crate::token::*;
use peekmore::{PeekMore, PeekMoreIterator};
use std::str::CharIndices;

#[derive(Debug)]
pub struct Scanner<'a> {
    source: &'a str,
    iter: PeekMoreIterator<CharIndices<'a>>,
    tokens: Vec<Token<'a>>,
    line: u32,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source: source,
            iter: source.char_indices().peekmore(),
            tokens: Vec::new(),
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) {
        loop {
            match self.iter.next() {
                Some((_, ' ')) => continue,
                Some((_, '\n')) => self.line = self.line + 1,
                Some((i, '(')) => self.add_token(TokenType::LeftParen, i, 1),
                Some((i, ')')) => self.add_token(TokenType::RightParen, i, 1),
                Some((i, '{')) => self.add_token(TokenType::LeftBrace, i, 1),
                Some((i, '}')) => self.add_token(TokenType::RightBrace, i, 1),
                Some((i, ',')) => self.add_token(TokenType::Comma, i, 1),
                Some((i, '.')) => self.add_token(TokenType::Dot, i, 1),
                Some((i, '-')) => self.add_token(TokenType::Minus, i, 1),
                Some((i, '+')) => self.add_token(TokenType::Plus, i, 1),
                Some((i, ';')) => self.add_token(TokenType::Semicolon, i, 1),
                Some((i, '*')) => self.add_token(TokenType::Star, i, 1),

                Some((i, '!')) => {
                    self.add_matches_token(TokenType::Bang, '=', TokenType::BangEqual, i)
                }
                Some((i, '=')) => {
                    self.add_matches_token(TokenType::Equal, '=', TokenType::EqulaEqual, i)
                }
                Some((i, '<')) => {
                    self.add_matches_token(TokenType::Less, '=', TokenType::LessEqual, i)
                }
                Some((i, '>')) => {
                    self.add_matches_token(TokenType::Greater, '=', TokenType::GreaterEqual, i)
                }

                Some((_, character)) => {
                    self.report(format!("Unexpect character [{}]", character).as_str())
                }

                None => break,
            }
        }
    }

    fn report(&self, message: &str) {
        println!("[line {}] Error: {}", self.line, message);
    }

    fn add_token(&mut self, token_type: TokenType, start: usize, len: usize) {
        self.tokens.push(Token {
            token_type: token_type,
            line: self.line,
            lexeme: Lexeme::String(&self.source[start..start + len]),
        });
        for _ in 1..len {
            self.iter.next();
        }
    }

    fn add_matches_token(
        &mut self,
        token_type: TokenType,
        matches_char: char,
        matches_token_type: TokenType,
        start: usize,
    ) {
        let (token_type_added, len) = match self.iter.peek() {
            Some((_, char)) if *char == matches_char => (matches_token_type, 2),
            _ => (token_type, 1),
        };
        self.add_token(token_type_added, start, len);
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
}
