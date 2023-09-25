//! Cursor for scanning source code.
use std::str::CharIndices;

pub const EOF_CHAR: char = '\0';
pub const NEWLINE: char = '\n';

/// Controls the position in source code while scanning.
pub struct Cursor<'src> {
    /// Original source code text.
    source: &'src str,
    /// An iterator over the characters and their byte positions
    /// in a string slice.
    chars: CharIndices<'src>,
    /// Previous character returned by the internal iterator.
    ///
    /// Store the result of the previous iteration so it's
    /// available on demand as the "current" state of the cursor.
    prev: Option<(usize, char)>,
    /// Count the encountered lines.
    line: usize,
}

impl<'src> Cursor<'src> {
    /// Construct cursor from given source code text.
    pub fn from_str(source: &'src str) -> Self {
        Cursor {
            source,
            chars: source.char_indices(),
            prev: Some((0, EOF_CHAR)),
            line: 0,
        }
    }

    /// Original source code text.
    #[inline(always)]
    pub fn source(&self) -> &'src str {
        self.source
    }

    /// Remaining source text to be consumed. Starting at the
    /// current position.
    pub fn rest(&self) -> &'src str {
        &self.source[self.position()..]
    }

    /// Current line number.
    #[allow(dead_code)] // TODO: Could be used later in compiler implementation
    pub fn line(&self) -> usize {
        self.line
    }

    /// Byte position of the current character.
    pub fn position(&self) -> usize {
        self.prev.map(|(i, _)| i).unwrap_or_else(|| self.source.len())
    }

    /// Current character in the iteration.
    ///
    /// If iteration has not started, will return end-of-file character.
    pub fn char(&self) -> char {
        self.prev.map(|(_, c)| c).unwrap_or(EOF_CHAR)
    }

    pub fn current(&self) -> Option<(usize, char)> {
        self.prev
    }

    /// Peek the next character without advancing the cursor.
    pub fn peek(&self) -> char {
        let mut chars = self.chars.clone();
        chars.next().map(|(_, c)| c).unwrap_or(EOF_CHAR)
    }

    /// Peek two characters ahead without advancing the cursor.
    pub fn peek2(&self) -> char {
        let mut chars = self.chars.clone();
        chars.next();
        chars.next().map(|(_, c)| c).unwrap_or(EOF_CHAR)
    }

    /// Peek the byte position of the next character.
    #[allow(dead_code)] // TODO: Review if peek_position() is necessary
    pub fn peek_position(&self) -> usize {
        // Byte position of next character is determined by number
        // of bytes taken up by the current character.
        //
        // Because of UTF-8 encoding, there is no easy way
        // to know the size of the current character except
        // advancing the iterator.
        let mut chars = self.chars.clone();
        chars.next().map(|(i, _)| i).unwrap_or_else(|| self.source.len())
    }

    /// Indicates whether the cursor is at the start of the source.
    pub fn at_start(&self) -> bool {
        match self.prev {
            Some((i, _)) => i == 0,
            None => false, // at end
        }
    }

    /// Indicates whether the cursor is at the end of the source.
    pub fn at_end(&self) -> bool {
        // let mut chars = self.chars.clone();
        // chars.next().is_none()
        self.prev.is_none()
    }

    /// Advances the cursor to the next character.
    ///
    /// Returns `None` if the cursor is end-of-file.
    pub fn bump(&mut self) -> Option<(usize, char)> {
        match self.chars.next() {
            Some((i, c)) => {
                // Convert index to smaller integer so
                // tuple fits into 64-bits.
                self.prev = Some((i, c));
                if c == NEWLINE {
                    self.line += 1;
                }
                Some((i, c))
            }
            None => {
                // Point the internal byte offset to one
                // element after the source text, so calls
                // to `offset` and `current` show that the
                // cursor is exhausted.
                self.prev = None;
                None
            }
        }
    }

    /// Advance the cursor, if the current character matches the given one.
    ///
    /// Returns `true` if there was a match.
    pub fn match_bump(&mut self, ch: char) -> bool {
        if ch == self.char() {
            self.bump();
            true
        } else {
            false
        }
    }

    /// Skip over the given number of characters.
    ///
    /// The name `skip` makes clippy unhappy at each call site.
    pub fn skip_n(&mut self, n: usize) {
        for _ in 0..n {
            self.bump();
        }
    }

    #[allow(dead_code)] // used for tests
    pub fn with_bump(mut self) -> Self {
        self.bump();
        self
    }
}

impl<'src> Iterator for Cursor<'src> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        self.bump()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_peek() {
        let mut cursor = Cursor::from_str("abcd");
        assert_eq!(cursor.peek(), 'a');
        assert_eq!(cursor.peek2(), 'b');

        assert_eq!(cursor.bump(), Some((0, 'a')));
        assert_eq!(cursor.bump(), Some((1, 'b')));

        assert_eq!(cursor.peek(), 'c');
        assert_eq!(cursor.peek2(), 'd');

        assert_eq!(cursor.bump(), Some((2, 'c')));

        assert_eq!(cursor.peek(), 'd');
        assert_eq!(cursor.peek2(), EOF_CHAR);

        assert_eq!(cursor.bump(), Some((3, 'd')));

        assert_eq!(cursor.peek(), EOF_CHAR);
        assert_eq!(cursor.peek2(), EOF_CHAR);
    }

    #[test]
    fn test_eof() {
        assert_eq!(Cursor::from_str("").with_bump().at_end(), true);
        assert_eq!(Cursor::from_str("abc").with_bump().at_end(), false);

        // Exhausted cursor must return EOF
        let mut cursor = Cursor::from_str("a");
        // Initial state
        assert_eq!(cursor.char(), EOF_CHAR);
        assert_eq!(cursor.position(), 0);
        cursor.bump();
        assert_eq!(cursor.char(), 'a');
        assert_eq!(cursor.position(), 0);
        cursor.bump();
        assert_eq!(cursor.char(), EOF_CHAR);
        assert_eq!(cursor.position(), 1);

        // Test case where string has explicit EOF sentinel.
        let mut cursor = Cursor::from_str("abc\0");
        assert_eq!(cursor.bump(), Some((0, 'a')));
        assert_eq!(cursor.char(), 'a');
        assert_eq!(cursor.position(), 0);

        assert_eq!(cursor.bump(), Some((1, 'b')));
        assert_eq!(cursor.char(), 'b');
        assert_eq!(cursor.position(), 1);

        assert_eq!(cursor.bump(), Some((2, 'c')));
        assert_eq!(cursor.char(), 'c');
        assert_eq!(cursor.position(), 2);

        assert_eq!(cursor.bump(), Some((3, EOF_CHAR)));
        assert_eq!(cursor.char(), EOF_CHAR); // explicit
        assert_eq!(cursor.position(), 3);

        assert_eq!(cursor.bump(), None);
        assert_eq!(cursor.char(), EOF_CHAR); // implicit
        assert_eq!(cursor.position(), 4);
    }

    #[test]
    fn test_skip() {
        let mut cursor = Cursor::from_str("abcdef").with_bump();
        cursor.skip_n(3);
        assert_eq!(cursor.position(), 3);
        assert_eq!(cursor.char(), 'd');
        assert_eq!(cursor.rest(), "def");
    }
}
