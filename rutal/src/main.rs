mod lexer;

use lexer::{Lexer, LexerError, OpCodeFull, SpannedToken, Token, Rune};
use thiserror::Error;

#[derive(Debug)]
struct Macro {
    name: String,
    items: Vec<SpannedToken>, // Hardlimit to 256 items
    refs: u16,
}

impl Macro {
    pub fn new<'a, 'b>(name: String, stream: &'b mut impl Iterator<Item = &'a SpannedToken>) -> Result<Macro, AssemblerError> {
        if !matches!(stream.take(1).nth(0), Some((Token::Rune(Rune::MacroStart), _, _))) {
            Err(AssemblerError::NoMacroStart(name))
        } else {
            let items: Vec<_> = stream
                .take_while(|(tok, _s, _len)| *tok != Token::Rune(Rune::MacroEnd)).collect();

            if items.len() > 64 {
                Err(AssemblerError::MacroTooLarge(name))
            } else {
                Ok(Macro {
                    name,
                    items: items.into_iter().cloned().collect(),
                    refs: 0,
                })
            }
        }
    }
}

#[derive(Debug)]
struct Label {
    name: String,
    addr: u16,
    refs: u16,
}

impl Label {
    pub fn new(name: String, addr: u16) -> Label {
        Label {
            name,
            addr,
            refs: 0,
        }
    }
}

#[derive(Debug)]
struct Program {
    /// Program Data
    data: [u8; 65536],
    ptr: u16,
    length: u16,
}

impl Program {
    pub fn new() -> Program {
        Program {
            data: [0; 65536],
            ptr: 0,
            length: 0,
        }
    }

    pub fn to_bytes(&self) -> &[u8] {
        &self.data[0x100..std::cmp::max(self.length, 0x100) as usize]
    }

    pub fn pushbyte(&mut self, b: u8, lit: bool) {
        if lit {
            self.pushbyte(0x80, false)
        }
        self.data[self.ptr as usize] = b;
        self.ptr += 1;
        self.length = self.ptr;
    }

    pub fn pushshort(&mut self, s: u16, lit: bool) {
        if lit {
            self.pushbyte(0x20, false)
        }
        self.pushbyte(((s >> 8) & 0xff) as u8, false);
        self.pushbyte((s & 0xff) as u8, false)
    }

    pub fn pushword<S: AsRef<str>>(&mut self, s: S) {
        s.as_ref().bytes().for_each(|b| {
            self.pushbyte(b, false)
        })
    }
}

#[derive(Debug, Error)]
pub enum AssemblerError {
    #[error("Macro too large: {0}\nPass 1 - Invalid macro: {0}\nAssembly: Failed to assemble rom.")]
    MacroTooLarge(String),
    #[error("Missing {{: {0}\nPass 1 - Invalid macro: {0}\nAssembly: Failed to assemble rom.")]
    NoMacroStart(String),
    #[error("Too many macros: {0}\nPass 1 - Invalid macro: {0}\nAssembly: Failed to assemble rom.")]
    TooManyMacros(String),
    #[error("Too many labels: {0}\nPass 1 - Invalid label: {0}\nAssembly: Failed to assemble rom.")]
    TooManyLabels(String),
    #[error("Pass 2 - Memory overwrite: {0}\nAssembly: Failed to assemble rom.")]
    MemoryOverwrite(Token),
    #[error("Address is not in zero page: {0}\nPass 2 - Unknown label: {0}\nAssembly: Failed to assemble rom.")]
    LabelNotOnZeropage(String),
    #[error("Address is too far: {0}\nPass 2 - Unknown label: {0}\nAssembly: Failed to assemble rom.")]
    AddressTooFar(String),
    #[error("Invalid macro: {1}\n{0}")]
    InvalidMacro(Box<AssemblerError>, String),
    #[error("Invalid token: {0}\nPass 2 - Unknown label: {0}\nAssembly: Failed to assemble rom.")]
    InvalidToken(Token),
}

#[derive(Debug)]
struct Assembler {
    program: Program,
    labels: Vec<Label>, // Hardlimit to Assembler::LABEL_MAX
    macros: Vec<Macro>, // Hardlimit to Assembler::MACRO_MAX
    addr: u16,
}

impl Assembler {
    const LABEL_MAX: usize = 512;
    const MACRO_MAX: usize = 256;

    pub fn new() -> Assembler {
        Assembler {
            program: Program::new(),
            labels: vec![],
            macros: vec![],
            addr: 0,
        }
    }

    pub fn find_macro<S: AsRef<str>>(&self, name: S) -> Option<&Macro> {
        self.macros.iter().find(|macr| macr.name == name.as_ref())
    }

    pub fn find_label<S: AsRef<str>>(&self, name: S) -> Option<&Label> {
        self.labels.iter().find(|labl| labl.name == name.as_ref())
    }

    pub fn find_macro_mut<S: AsRef<str>>(&mut self, name: S) -> Option<&mut Macro> {
        self.macros.iter_mut().find(|macr| macr.name == name.as_ref())
    }

    pub fn find_label_mut<S: AsRef<str>>(&mut self, name: S) -> Option<&mut Label> {
        self.labels.iter_mut().find(|labl| labl.name == name.as_ref())
    }

    pub fn walk_token(&self, token: Token) -> u16 {
        match token {
            Token::Rune(Rune::RawByteShort(hex)) => hex.len() as u16 / 2,
            Token::Rune(Rune::RawChar(_)) => 1,
            Token::Rune(Rune::LiteralAddrZP(_)
                | Rune::LiteralAddrRel(_)
                | Rune::RawAddr(_)) => 2,
            Token::Rune(Rune::LiteralAddrAbs(_)) => 3,
            Token::Rune(Rune::PadRelative(padding)) => u16::from_str_radix(&padding, 16).unwrap(),
            Token::Rune(Rune::RawWord(word)) => word.bytes().count() as u16,
            Token::Rune(Rune::LiteralHex(hex)) => if hex.len() == 4 { 3 } else { 2 },
            Token::OpCode(_) => 1,
            Token::Rune(Rune::MacroStart) => {
                eprintln!("Invalid token: {{");
                0
            },
            Token::Rune(Rune::MacroEnd) => {
                eprintln!("Invalid token: }}");
                0
            },
            Token::UnknownMacro(macr) => {
                if let Some(macr) = self.find_macro(&macr) {
                    macr.items.iter().map(|(t, _, _)| self.walk_token(t.clone())).sum()
                } else {
                    eprintln!("Invalid token: {}", macr);
                    0
                }
            }
            tok => {
                eprintln!("Invalid token: {}", tok);
                0
            }
        }
    }

    fn parse_token(&mut self, token: Token) -> Result<(), AssemblerError> {
        match token {
            Token::Rune(Rune::LiteralAddrZP(label)) if self.find_label(&label).is_some() => {
                let addr = if let Some(label) = self.find_label_mut(label) {
                    if label.addr > 0xff {
                        return Err(AssemblerError::LabelNotOnZeropage(label.name.clone()))
                    }
                    label.refs += 1;
                    Some(label.addr)
                } else {
                    None
                };
                if let Some(addr) = addr {
                    self.program.pushbyte((addr & 0xff) as u8, true);
                }
            }
            Token::Rune(Rune::LiteralAddrRel(label)) if self.find_label(&label).is_some() => {
                let cptr = self.program.ptr as i16;
                let off = if let Some(label) = self.find_label_mut(&label) {
                    let off = label.addr as i16 - cptr - 3;
                    if off < -126 || off > 126 {
                        return Err(AssemblerError::AddressTooFar(label.name.clone()))
                    }
                    label.refs += 1;
                    Some(off)
                } else {
                    None
                };
                if let Some(off) = off {
                    self.program.pushbyte(off as u8, true);
                }
            }
            Token::Rune(Rune::RawAddr(label)) if self.find_label(&label).is_some() => {
                let addr = if let Some(label) = self.find_label_mut(label) {
                    label.refs += 1;
                    Some(label.addr)
                } else {
                    None
                };
                if let Some(addr) = addr {
                    self.program.pushshort(addr, false);
                }
            }
            Token::Rune(Rune::LiteralAddrAbs(label)) if self.find_label(&label).is_some() => {
                let addr = if let Some(label) = self.find_label_mut(label) {
                    label.refs += 1;
                    Some(label.addr)
                } else {
                    None
                };
                if let Some(addr) = addr {
                    self.program.pushshort(addr, true);
                }
            }
            Token::OpCode(opc) => self.program.pushbyte(opc.to_byte(), false),
            Token::Rune(Rune::RawWord(wrd)) => self.program.pushword(wrd),
            Token::Rune(Rune::RawChar(chr)) => self.program.pushbyte(chr.bytes().nth(0).unwrap(), false),
            Token::Rune(Rune::LiteralHex(hex)) => if hex.bytes().count() == 1 {
                self.program.pushbyte(hex.bytes().nth(0).unwrap(), true)
            } else if hex.len() == 2 {
                let hex = u8::from_str_radix(&hex, 16).unwrap();
                self.program.pushbyte(hex, true);
            } else if hex.len() == 4 {
                let hex = u16::from_str_radix(&hex, 16).unwrap();
                self.program.pushshort(hex, true);
            },
            Token::Rune(Rune::RawByteShort(bs)) => if bs.len() == 2 {
                let bs = u8::from_str_radix(&bs, 16).unwrap();
                self.program.pushbyte(bs, false);
            } else if bs.len() == 4 {
                let bs = u16::from_str_radix(&bs, 16).unwrap();
                self.program.pushshort(bs, false);
            },
            Token::UnknownMacro(macr) if self.find_macro(&macr).is_some() => {
                let commands = if let Some(macr) = self.find_macro_mut(macr.clone()) {
                    macr.items.iter().map(|(t, _, _)| t.clone()).collect()
                } else {
                    vec![]
                };

                commands.into_iter().try_fold((), |_, f| {
                    self.parse_token(f)
                }).map_err(|e| AssemblerError::InvalidMacro(Box::new(e), macr.clone()))?;
            }
            tok => return Err(AssemblerError::InvalidToken(tok))
        }
        Ok(())
    }

    // Declaration phase
    pub fn pass1(&mut self, tokens: &[SpannedToken]) -> Result<(), AssemblerError> {
        let mut iter = tokens.into_iter();
        while let Some((tok, start, len)) = iter.next().cloned() {
            match tok {
                Token::Rune(Rune::PadAbsolute(padding)) => {
                    let pad = u16::from_str_radix(&padding, 16).unwrap();
                    self.addr = pad;
                }
                Token::Rune(Rune::MacroDefine(name)) => {
                    let macr = Macro::new(name.clone(), &mut iter)?;
                    self.macros.push(macr);
                    if self.macros.len() > Self::MACRO_MAX {
                        return Err(AssemblerError::TooManyMacros(name.clone()))
                    }
                }
                Token::Rune(Rune::LabelDefine(name)) => {
                    let label = Label::new(name.clone(), self.addr);
                    self.labels.push(label);
                    if self.labels.len() > Self::LABEL_MAX {
                        return Err(AssemblerError::TooManyLabels(name.clone()))
                    }
                }
                tok => self.addr += self.walk_token(tok)
            }
        }
        Ok(())
    }

    // Program writing phase
    pub fn pass2(&mut self, tokens: &[SpannedToken]) -> Result<(), AssemblerError> {
        let mut iter = tokens.into_iter();
        while let Some((tok, start, len)) = iter.next().cloned() {
            match tok.clone() {
                Token::Rune(Rune::MacroDefine(_)) => {
                    Macro::new("".into(), &mut iter)?; // Eat the macro
                },
                Token::Rune(Rune::LabelDefine(_)) => {},
                Token::Rune(Rune::PadAbsolute(padding)) => {
                    let pad = u16::from_str_radix(&padding, 16).unwrap();
                    if self.program.length != 0 && pad < self.program.ptr {
                        return Err(AssemblerError::MemoryOverwrite(tok))
                    }
                    self.program.ptr = pad;
                }
                Token::Rune(Rune::PadRelative(padding)) => {
                    let pad = u16::from_str_radix(&padding, 16).unwrap();
                    self.program.ptr += pad;
                }
                tok => self.parse_token(tok)?
            }
        }
        Ok(())
    }

    pub fn program(&self) -> &Program { &self.program }
    pub fn labels(&self) -> &[Label] { &self.labels }
    pub fn macros(&self) -> &[Macro] { &self.macros }
}

use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        panic!("usage: input.tal output.rom");
    }
    let mut source = String::new();
    match std::fs::File::open(&args[1]) {
        Ok(mut file) => {
            use std::io::Read;
            match file.read_to_string(&mut source) {
                Ok(n) => {
                    eprintln!("Load: Loaded {} bytes of source.", n);
                },
                Err(e) => {
                    panic!("Load: Failed to open source.");
                }
            }
        },
        Err(_) => {
            panic!("Load: Failed to open source.");
        }
    };

    let lexer = Lexer::new(&source);
    let tokens_or_err: Result<Vec<_>, LexerError> = lexer.into_iter().collect();
    let tokens = match tokens_or_err {
        Ok(tokens) => tokens,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };
    for (token, start, len) in &tokens {
        println!("{:08x}[{:02x}]{: >64}", start, len, format!("{}", token));
    }

    let mut assembler = Assembler::new();
    match assembler.pass1(&tokens) {
        Ok(()) => {},
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };
    match assembler.pass2(&tokens) {
        Ok(()) => {},
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };
    // println!("{:?}", assembler);

    {
        let mut file = std::fs::File::create(&args[2]).unwrap();
        use std::io::Write;
        file.write_all(assembler.program.to_bytes()).unwrap();
    }

    println!("Assembled {}({} bytes), {} labels, {} macros.", args[2], 
        assembler.program.to_bytes().len(), 
        assembler.labels().len(),
        assembler.macros().len()
    );
}
