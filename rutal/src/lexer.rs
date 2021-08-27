use std::iter::{Enumerate, Peekable};
use std::str::Chars;
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpCode {
    Break,
    Increment,
    Pop,
    Duplicate,
    Nip,
    Swap,
    Over,
    Rotate,

    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    Jump,
    JumpCond,
    JumpStash,
    Stash,

    LoadZeropage,
    StoreZeropage,
    LoadRelative,
    StoreRelative,
    LoadAbsolute,
    StoreAbsolute,
    DeviceIn,
    DeviceOut,

    Add,
    Subtract,
    Multiply,
    Divide,
    And,
    Or,
    ExclusiveOr,
    Shift,
}

impl OpCode {
    pub fn to_byte(&self) -> u8 {
        match self {
            OpCode::Break         => 0x00,
            OpCode::Increment     => 0x01,
            OpCode::Pop           => 0x02,
            OpCode::Duplicate     => 0x03,
            OpCode::Nip           => 0x04,
            OpCode::Swap          => 0x05,
            OpCode::Over          => 0x06,
            OpCode::Rotate        => 0x07,
            OpCode::Equal         => 0x08,
            OpCode::NotEqual      => 0x09,
            OpCode::GreaterThan   => 0x0a,
            OpCode::LessThan      => 0x0b,
            OpCode::Jump          => 0x0c,
            OpCode::JumpCond      => 0x0d,
            OpCode::JumpStash     => 0x0e,
            OpCode::Stash         => 0x0f,
            OpCode::LoadZeropage  => 0x10,
            OpCode::StoreZeropage => 0x11,
            OpCode::LoadRelative  => 0x12,
            OpCode::StoreRelative => 0x13,
            OpCode::LoadAbsolute  => 0x14,
            OpCode::StoreAbsolute => 0x15,
            OpCode::DeviceIn      => 0x16,
            OpCode::DeviceOut     => 0x17,
            OpCode::Add           => 0x18,
            OpCode::Subtract      => 0x19,
            OpCode::Multiply      => 0x1a,
            OpCode::Divide        => 0x1b,
            OpCode::And           => 0x1c,
            OpCode::Or            => 0x1d,
            OpCode::ExclusiveOr   => 0x1e,
            OpCode::Shift         => 0x1f,
        }
    }
}

impl std::fmt::Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let code = match self {
            OpCode::Break         => "LIT",
            OpCode::Increment     => "INC",
            OpCode::Pop           => "POP",
            OpCode::Duplicate     => "DUP",
            OpCode::Nip           => "NIP",
            OpCode::Swap          => "SWP",
            OpCode::Over          => "OVR",
            OpCode::Rotate        => "ROT",
            OpCode::Equal         => "EQU",
            OpCode::NotEqual      => "NEQ",
            OpCode::GreaterThan   => "GTH",
            OpCode::LessThan      => "LTH",
            OpCode::Jump          => "JMP",
            OpCode::JumpCond      => "JCN",
            OpCode::JumpStash     => "JSR",
            OpCode::Stash         => "STH",
            OpCode::LoadZeropage  => "LDZ",
            OpCode::StoreZeropage => "STZ",
            OpCode::LoadRelative  => "LDR",
            OpCode::StoreRelative => "STR",
            OpCode::LoadAbsolute  => "LDA",
            OpCode::StoreAbsolute => "STA",
            OpCode::DeviceIn      => "DEI",
            OpCode::DeviceOut     => "DEO",
            OpCode::Add           => "ADD",
            OpCode::Subtract      => "SUB",
            OpCode::Multiply      => "MUL",
            OpCode::Divide        => "DIV",
            OpCode::And           => "AND",
            OpCode::Or            => "ORA",
            OpCode::ExclusiveOr   => "EOR",
            OpCode::Shift         => "SFT",
        };
        write!(f, "{}", code)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Rune {
    MacroDefine(String), // %
    MacroStart, // {
    MacroEnd, // }
    PadAbsolute(String), // |
    PadRelative(String), // $
    LabelDefine(String), // @
    //SublabelDefine(String), // &
    LiteralHex(String), // #
    LiteralAddrZP(String), // .
    LiteralAddrRel(String), // ,
    LiteralAddrAbs(String), // ;
    RawAddr(String), // :
    RawChar(String), // '
    RawWord(String), // "
    RawByteShort(String), // <Nothing>
}

impl std::fmt::Display for Rune {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Rune::MacroDefine(name) => write!(f, "%{}", name),
            Rune::MacroStart => write!(f, "{{"),
            Rune::MacroEnd => write!(f, "}}"),
            Rune::PadAbsolute(by) => write!(f, "|{}", by),
            Rune::PadRelative(by) => write!(f, "${}", by),
            Rune::LabelDefine(lab) => write!(f, "@{}", lab),
            Rune::LiteralHex(hex) => write!(f, "#{}", hex),
            Rune::LiteralAddrZP(addr) => write!(f, ".{}", addr),
            Rune::LiteralAddrRel(addr) => write!(f, ",{}", addr),
            Rune::LiteralAddrAbs(addr) => write!(f, ";{}", addr),
            Rune::RawAddr(addr) => write!(f, ":{}", addr),
            Rune::RawChar(chr) => write!(f, "'{}", chr),
            Rune::RawWord(wrd) => write!(f, "\"{}", wrd),
            Rune::RawByteShort(bs) => write!(f, "{}", bs),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OpCodeFull {
    pub op: OpCode,
    pub short: bool,
    pub keep: bool,
    pub ret: bool
}

impl OpCodeFull {
    pub fn to_byte(&self) -> u8 {
        self.op.to_byte() |
        (self.short as u8) << 5 |
        (self.ret as u8) << 6 |
        (self.keep as u8) << 7
    }
}

impl std::fmt::Display for OpCodeFull {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.op == OpCode::Break && (self.short || self.ret || self.keep) == false {
            write!(f, "BRK")
        } else {
            write!(f, "{}{}{}{}", 
                self.op, 
                if self.short { "2" } else { "" },
                if self.keep { "k" } else { "" },
                if self.ret { "r" } else { "" }
            )
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    OpCode(OpCodeFull),
    Rune(Rune),
    UnknownMacro(String),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::OpCode(op) => write!(f, "{}", op),
            Token::Rune(r) => write!(f, "{}", r),
            Token::UnknownMacro(m) => write!(f, "{}", m),
        }
    }
}

// token, start, len
pub type SpannedToken = (Token, usize, usize);

#[derive(Debug)]
pub struct Lexer<'a> {
    source: Peekable<Enumerate<Chars<'a>>>,

    start: usize,
    current: usize,
    current_char: Option<char>,

    labels: Vec<String>,
    macros: Vec<String>,
    scope: Option<String>,
}

#[derive(Debug, Error)]
pub enum LexerError {
    #[error("Macro name is hex number: {0}\nPass 1 - Invalid macro: {0}\nAssembly: Failed to assemble rom.")]
    MacroNameHex(String),
    #[error("Macro name is invalid: {0}\nPass 1 - Invalid macro: {0}\nAssembly: Failed to assemble rom.")]
    MacroNameInvalid(String),
    #[error("Macro duplicate: {0}\nPass 1 - Invalid macro: {0}\nAssembly: Failed to assemble rom.")]
    MacroNameDuplicate(String),
    #[error("Pass 1 - Invalid padding: {0}\nAssembly: Failed to assemble rom.")]
    InvalidPadding(String),
    #[error("Label name is hex number: {0}\nPass 1 - Invalid {1}: {0}\nAssembly: Failed to assemble rom.")]
    LabelNameHex(String, &'static str),
    #[error("Label name is invalid: {0}\nPass 1 - Invalid {1}: {0}\nAssembly: Failed to assemble rom.")]
    LabelNameInvalid(String, &'static str),
    #[error("Label duplicate: {0}\nPass 1 - Invalid {1}: {0}\nAssembly: Failed to assemble rom.")]
    LabelNameDuplicate(String, &'static str),
    #[error("Invalid hexadecimal literal: {0}\nPass 2 - Unknown label: {0}\nAssembly: Failed to assemble rom.")]
    InvalidHexLiteral(String),
    #[error("Unknown token: {0}\nPass 2 - Unknown label: {0}\nAssembly: Failed to assemble rom.")]
    UnknownToken(char),
    #[error("Invalid addressing: {0}\nPass 2 - Unknown label: {0}\nAssembly: Failed to assemble rom.")]
    InvalidAddressing(String),
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Lexer<'a> {
        Lexer {
            source: s.chars().enumerate().peekable(),

            start: 0,
            current: 0,
            current_char: None,

            labels: vec![],
            macros: vec![],
            scope: None,
        }
    }

    fn start_token(&mut self) {
        if let Some((start, c)) = self.source.next() {
            self.start = start;
            self.current = start;
            self.current_char = Some(c);
        } else {
            self.current_char = None;
        }
    }

    fn advance(&mut self) {
        if let Some((start, c)) = self.source.next() {
            self.current = start;
            self.current_char = Some(c);
        } else {
            self.current_char = None;
        }
    }

    fn opcode_pattern<S: AsRef<str>>(loo: S) -> Option<Token> {
        let loo = loo.as_ref();

        let opcode = if loo.len() < 3 {
            loo
        } else {
            &loo[..3]
        };

        let flags = if loo.len() > 3 {
            &loo[3..]
        } else {
            ""
        };

        let opcode = match opcode {
            "BRK" => Some(OpCode::Break),
            "LIT" => Some(OpCode::Break),
            "INC" => Some(OpCode::Increment),
            "POP" => Some(OpCode::Pop),
            "DUP" => Some(OpCode::Duplicate),
            "NIP" => Some(OpCode::Nip),
            "SWP" => Some(OpCode::Swap),
            "OVR" => Some(OpCode::Over),
            "ROT" => Some(OpCode::Rotate),

            "EQU" => Some(OpCode::Equal),
            "NEQ" => Some(OpCode::NotEqual),
            "GTH" => Some(OpCode::GreaterThan),
            "LTH" => Some(OpCode::LessThan),
            "JMP" => Some(OpCode::Jump),
            "JCN" => Some(OpCode::JumpCond),
            "JSR" => Some(OpCode::JumpStash),
            "STH" => Some(OpCode::Stash),

            "LDZ" => Some(OpCode::LoadZeropage),
            "STZ" => Some(OpCode::StoreZeropage),
            "LDR" => Some(OpCode::LoadRelative),
            "STR" => Some(OpCode::StoreRelative),
            "LDA" => Some(OpCode::LoadAbsolute),
            "STA" => Some(OpCode::StoreAbsolute),
            "DEI" => Some(OpCode::DeviceIn),
            "DEO" => Some(OpCode::DeviceOut),

            "ADD" => Some(OpCode::Add),
            "SUB" => Some(OpCode::Subtract),
            "MUL" => Some(OpCode::Multiply),
            "DIV" => Some(OpCode::Divide),
            "AND" => Some(OpCode::And),
            "ORA" => Some(OpCode::Or),
            "EOR" => Some(OpCode::ExclusiveOr),
            "SFT" => Some(OpCode::Shift),

            _ => None
        };

        let short = flags.contains("2");
        let keep = flags.contains("k");
        let ret = flags.contains("r");

        opcode.map(|op| Token::OpCode(OpCodeFull {
            op,
            short,
            keep,
            ret
        }))
    }

    fn peek_char(&mut self) -> Option<char> {
        self.source.peek().map(|(_, c)| *c)
    }

    fn lex_string(&mut self, c: Option<char>) -> String {
        let mut loo = String::new();
        if let Some(c) = c {
            loo.push(c);
        }
        while self.peek_char().is_some() 
            //&& !"%{}|$#.,;:'\"".contains(self.peek_char().unwrap()) 
            && !self.peek_char().unwrap().is_whitespace()
            && loo.len() < 64
        {
            self.advance();
            loo.push(self.current_char.unwrap());
        }


        if loo.len() == 63 {
            panic!("Warning: token beginning with {} is too long", loo.chars().nth(0).unwrap());
        }
        loo[..std::cmp::min(63, loo.len())].to_string()
    }

    fn skip_whitespace(&mut self) {
        while self.current_char.is_some() && self.current_char.unwrap().is_whitespace() {
            self.advance();
        }
        self.start = self.current;
    }

    fn is_hex<S: AsRef<str>>(s: S) -> bool {
        s.as_ref().chars().all(|c| c.is_ascii_hexdigit())
    }

    fn process_address(&mut self) -> Result<String, LexerError> {
        let mut arg = self.lex_string(None);
        if arg.contains('&') && !arg.starts_with('&') ||
            arg.contains('@') && !arg.starts_with('@') {
            return Err(LexerError::InvalidAddressing(arg))
        }
        if arg.starts_with('&') && arg.len() > 1 {
            arg = format!("{}/{}", self.scope.clone().unwrap_or_default(), &arg[1..]);
        }
        if arg.starts_with('@') && arg.len() > 1 {
            arg = arg[1..].to_string();
        }
        Ok(arg)
    }

    fn lex(&mut self) -> Option<Result<SpannedToken, LexerError>> {
        self.start_token();
        let mut tok = None;
        let mut tryal = true;
        while tryal && tok.is_none() {
            match self.current_char {
                None => tryal = false,
                Some(c) if c.is_whitespace() => self.skip_whitespace(),
                Some('(') => {
                    while self.current_char != Some(')') && self.current_char.is_some() {
                        self.advance();
                    }
                    self.advance(); // Consume the )
                },
                Some('%') => {
                    let arg = self.lex_string(None);
                    if Lexer::is_hex(&arg) && arg.len() % 2 == 0 {
                        return Some(Err(LexerError::MacroNameHex(arg)))
                    }
                    if Lexer::opcode_pattern(&arg).is_some() || arg.len() == 0 {
                        return Some(Err(LexerError::MacroNameInvalid(arg)))
                    }
                    if self.macros.contains(&arg) {
                        return Some(Err(LexerError::MacroNameDuplicate(arg)))
                    }
                    self.macros.push(arg.clone());
                    tok = Some(Token::Rune(Rune::MacroDefine(arg)))
                },
                Some('{') => tok = Some(Token::Rune(Rune::MacroStart)),
                Some('}') => tok = Some(Token::Rune(Rune::MacroEnd)),
                Some('|') => {
                    let arg = self.lex_string(None);
                    if !Lexer::is_hex(&arg) {
                        return Some(Err(LexerError::InvalidPadding(arg)))
                    }
                    tok = Some(Token::Rune(Rune::PadAbsolute(arg)))
                },
                Some('$') => {
                    let arg = self.lex_string(None);
                    if !Lexer::is_hex(&arg) {
                        return Some(Err(LexerError::InvalidPadding(arg)))
                    }
                    tok = Some(Token::Rune(Rune::PadRelative(arg)))
                },
                Some('@') => {
                    let arg = self.lex_string(None);
                    if Lexer::is_hex(&arg) && arg.len() % 2 == 0 {
                        return Some(Err(LexerError::LabelNameHex(arg, "label")))
                    }
                    if Lexer::opcode_pattern(&arg).is_some() || arg.len() == 0 {
                        return Some(Err(LexerError::LabelNameInvalid(arg, "label")))
                    }
                    if self.labels.contains(&arg) {
                        return Some(Err(LexerError::LabelNameDuplicate(arg, "label")))
                    }
                    self.labels.push(arg.clone());
                    self.scope = Some(arg.clone());
                    tok = Some(Token::Rune(Rune::LabelDefine(arg)))
                },
                Some('&') => {
                    let sl = self.lex_string(None);
                    let arg = format!("{}/{}", self.scope.clone().unwrap_or("".into()), sl);
                    if Lexer::is_hex(&sl) && sl.len() % 2 == 0 {
                        return Some(Err(LexerError::LabelNameHex(arg, "sublabel")))
                    }
                    if Lexer::opcode_pattern(&sl).is_some() || sl.len() == 0 {
                        return Some(Err(LexerError::LabelNameInvalid(arg, "sublabel")))
                    }
                    if self.labels.contains(&arg) {
                        return Some(Err(LexerError::LabelNameDuplicate(arg, "sublabel")))
                    }
                    self.labels.push(arg.clone());
                    tok = Some(Token::Rune(Rune::LabelDefine(arg)))
                },
                Some('#') => {
                    let arg = self.lex_string(None);
                    if !(arg.len() == 1 || (Lexer::is_hex(&arg) && (arg.len() == 2 || arg.len() == 4))) {
                        return Some(Err(LexerError::InvalidHexLiteral(arg)))
                    }
                    tok = Some(Token::Rune(Rune::LiteralHex(arg)))
                },
                Some('.') => {
                    let arg = self.process_address();
                    if let Err(e) = arg {
                        return Some(Err(e))
                    } else if let Ok(arg) = arg {
                        tok = Some(Token::Rune(Rune::LiteralAddrZP(arg)))
                    }
                },
                Some(',') => {
                    let arg = self.process_address();
                    if let Err(e) = arg {
                        return Some(Err(e))
                    } else if let Ok(arg) = arg {
                        tok = Some(Token::Rune(Rune::LiteralAddrRel(arg)))
                    }
                },
                Some(';') => {
                    let arg = self.process_address();
                    if let Err(e) = arg {
                        return Some(Err(e))
                    } else if let Ok(arg) = arg {
                        tok = Some(Token::Rune(Rune::LiteralAddrAbs(arg)))
                    }
                },
                Some(':') => {
                    let arg = self.process_address();
                    if let Err(e) = arg {
                        return Some(Err(e))
                    } else if let Ok(arg) = arg {
                        tok = Some(Token::Rune(Rune::RawAddr(arg)))
                    }
                },
                Some('\'') => {
                    let arg = self.lex_string(None);
                    tok = Some(Token::Rune(Rune::RawChar(arg)))
                },
                Some('"') => {
                    let arg = self.lex_string(None);
                    tok = Some(Token::Rune(Rune::RawWord(arg)))
                },
                Some('[') => self.advance(),
                Some(']') => self.advance(),
                Some(c) => {
                    // Parse a raw short/byte, macro, or opcode
                    let loo = self.lex_string(Some(c));

                    tok = Some(Lexer::opcode_pattern(&loo).unwrap_or(
                        if Lexer::is_hex(&loo) && (loo.len() == 2 || loo.len() == 4) {
                            Token::Rune(Rune::RawByteShort(loo))
                        } else {
                            Token::UnknownMacro(loo)
                        }
                    ));
                },
            }
        }
        tok.map(|tt| Ok((tt, self.start, self.current - self.start + 1)))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<SpannedToken, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex()
    }
}