use lalrpop_util::lalrpop_mod;
use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufReader};
use std::str::FromStr;

// Pull in the parser module
lalrpop_mod!(pub xtrakcad); // synthesized by LALRPOP

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Scale {
    HO,
    N,
    O,
    G,
}
#[derive(Debug)]
pub struct Layout {
}

#[derive(Debug, PartialEq, Clone)]
pub enum LayoutError {
    ParseError(String),
    FileError(String),
    IOError(String),
}

use std::fmt;

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Clone, Debug)]
pub enum Tok {
    EOL,
    DOT,
    STRINGTOEOL(String),
    NULL,
    STRING(String),
    UINTEGER(u32),
    FLOAT(f64),
    A,
    ADJUSTABLE,
    ASPECT,
    B,
    BEZIER,
    BLOCK,
    BZRLIN,
    C,
    CAR,
    CONTROL,
    CORNU,
    CURRENT,
    CURVE,
    D,
    DRAW,
    E,
    ENDBLOCK,
    ENDSEGS,
    ENDSIGNAL,
    ENDTRACKS,
    F,
    G,
    H,
    HO,
    J,
    JOINT,
    L,
    LAYERS,
    M,
    MAIN,
    MAPSCALE,
    N,
    NOTE,
    O,
    P,
    PIER,
    Q,
    ROOMSIZE,
    S,
    SCALE,
    SENSOR,
    SIGNAL,
    STRAIGHT,
    STRUCTURE,
    SUBSEGS,
    SUBSEND,
    SWITCHMOTOR,
    T,
    TEXT,
    TITLE,
    TRK,
    TURNOUT,
    TURNTABLE,
    VERSION,
    W,
    X,
    Y,
    Z,
}

impl fmt::Display for Tok {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Tok::EOL => write!(f,"Tok::EOL" ),
            Tok::DOT => write!(f,"Tok::DOT" ),
            Tok::STRINGTOEOL(s) => write!(f,"Tok::STRINGTOEOL({})",s ),
            Tok::NULL => write!(f,"Tok::NULL" ),
            Tok::STRING(s) => write!(f,"Tok::STRING({})", s),
            Tok::UINTEGER(u) => write!(f,"Tok::UINTEGER({})",u ),
            Tok::FLOAT(flo) => write!(f,"Tok::FLOAT({})", flo ),
            Tok::A => write!(f,"Tok::A" ),
            Tok::ADJUSTABLE => write!(f,"Tok::ADJUSTABLE" ),
            Tok::ASPECT => write!(f,"Tok::ASPECT" ),
            Tok::B => write!(f,"Tok::B" ),
            Tok::BEZIER => write!(f,"Tok::BEZIER" ),
            Tok::BLOCK => write!(f,"Tok::BLOCK" ),
            Tok::BZRLIN => write!(f,"Tok::BZRLIN" ),
            Tok::C => write!(f,"Tok::C" ),
            Tok::CAR => write!(f,"Tok::CAR" ),
            Tok::CONTROL => write!(f,"Tok::CONTROL" ),
            Tok::CORNU => write!(f,"Tok::CORNU" ),
            Tok::CURRENT => write!(f,"Tok::CURRENT" ),
            Tok::CURVE => write!(f,"Tok::CURVE" ),
            Tok::D => write!(f,"Tok::D" ),
            Tok::DRAW => write!(f,"Tok::DRAW" ),
            Tok::E => write!(f,"Tok::E" ),
            Tok::ENDBLOCK => write!(f,"Tok::ENDBLOCK" ),
            Tok::ENDSEGS => write!(f,"Tok::ENDSEGS" ),
            Tok::ENDSIGNAL => write!(f,"Tok::ENDSIGNAL" ),
            Tok::ENDTRACKS => write!(f,"Tok::ENDTRACKS" ),
            Tok::F => write!(f,"Tok::F" ),
            Tok::G => write!(f,"Tok::G" ),
            Tok::H => write!(f,"Tok::H" ),
            Tok::HO => write!(f,"Tok::HO" ),
            Tok::J => write!(f,"Tok::J" ),
            Tok::JOINT => write!(f,"Tok::JOINT" ),
            Tok::L => write!(f,"Tok::L" ),
            Tok::LAYERS => write!(f,"Tok::LAYERS" ),
            Tok::M => write!(f,"Tok::M" ),
            Tok::MAIN => write!(f,"Tok::MAIN" ),
            Tok::MAPSCALE => write!(f,"Tok::MAPSCALE" ),
            Tok::N => write!(f,"Tok::N" ),
            Tok::NOTE => write!(f,"Tok::NOTE" ),
            Tok::O => write!(f,"Tok::O" ),
            Tok::P => write!(f,"Tok::P" ),
            Tok::PIER => write!(f,"Tok::PIER" ),
            Tok::Q => write!(f,"Tok::Q" ),
            Tok::ROOMSIZE => write!(f,"Tok::ROOMSIZE" ),
            Tok::S => write!(f,"Tok::S" ),
            Tok::SCALE => write!(f,"Tok::SCALE" ),
            Tok::SENSOR => write!(f,"Tok::SENSOR" ),
            Tok::SIGNAL => write!(f,"Tok::SIGNAL" ),
            Tok::STRAIGHT => write!(f,"Tok::STRAIGHT" ),
            Tok::STRUCTURE => write!(f,"Tok::STRUCTURE" ),
            Tok::SUBSEGS => write!(f,"Tok::SUBSEGS" ),
            Tok::SUBSEND => write!(f,"Tok::SUBSEND" ),
            Tok::SWITCHMOTOR => write!(f,"Tok::SWITCHMOTOR" ),
            Tok::T => write!(f,"Tok::T" ),
            Tok::TEXT => write!(f,"Tok::TEXT" ),
            Tok::TITLE => write!(f,"Tok::TITLE" ),
            Tok::TRK => write!(f,"Tok::TRK" ),
            Tok::TURNOUT => write!(f,"Tok::TURNOUT" ),
            Tok::TURNTABLE => write!(f,"Tok::TURNTABLE" ),
            Tok::VERSION => write!(f,"Tok::VERSION" ),
            Tok::W => write!(f,"Tok::W" ),
            Tok::X => write!(f,"Tok::X" ),
            Tok::Y => write!(f,"Tok::Y" ),
            Tok::Z => write!(f,"Tok::Z" ),
        }
    }
}

impl Tok {
    pub fn StringValue(self) -> String {
        match self {
            Tok::STRING(s) => s,
            _        => String::new(),
        }
    }
    pub fn U32Value(self) -> u32 {
        match self {
            Tok::UINTEGER(u) => u,
            _           => 0,
        }
    }
    pub fn F64Value(self) -> f64 {
        match self {
            Tok::FLOAT(f) => f,
            _        => 0.0,
        }
    }
}

#[derive(Debug, PartialEq, Clone)] 
pub enum LexicalError {
    UnTerminatedString,
    UnknownKeyword(String),
    IOError(String),
    UnknownCharacter(char),
}

impl fmt::Display for LexicalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexicalError::UnTerminatedString => write!(f, "Lexical Error: unterminated string"),
            LexicalError::UnknownKeyword(word) => write!(f, "Lexical Error: unknown keyword: {}",word),
            LexicalError::IOError(message) =>
                write!(f, "Lexical Error: error reading file: {}",message),
            LexicalError::UnknownCharacter(ch) =>
                write!(f, "Lexical Error: unknown character: {}",ch),
        }
    }
}

//impl fmt::Display for ParseError<usize, Tok, LexicalError> {
//    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//    }
//}    

//use std::str::Chars;
//use std::iter::Peekable;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ScanEOL {
    Off,
    Maybe,
    On,
}


pub struct Lexer<'input> {
    reader: &'input mut BufReader<File>,
    lineno: u32,
    first_column: usize,
    current_column: usize,
    peekChar: Option<char>,
    scaneol: ScanEOL,
    floatenable: bool,
}

impl<'input> Lexer<'input> {
    pub fn new(reader: &'input mut BufReader<File>) -> Self {
        Lexer { reader: reader, lineno: 1, first_column: 0, 
                current_column: 0, peekChar: None, scaneol: ScanEOL::Off, 
                floatenable: true }
    }
    fn PeekChar(&mut self) -> io::Result<Option<char>> {
        if self.peekChar.is_none() {
            let mut buffer: [u8; 1] = [0; 1];
            let status = self.reader.read(&mut buffer)?;
            if status > 0 {
                self.peekChar = Some(buffer[0] as char);
            }
        }
        Ok(self.peekChar)
    }
    fn ReadChar(&mut self) -> io::Result<Option<char>> {
        if self.peekChar.is_none() {self.PeekChar()?;}
        let result = self.peekChar;
        self.peekChar = None;
        Ok(result)
    }
}
#[derive(Debug, PartialEq, Copy, Clone, Default)]
pub struct FileLocation {
    lineno: u32,
    column: usize,
}

impl FileLocation {
    pub fn new(l: u32, c: usize) -> Self {
        Self {lineno: l, column: c }
    }
}

impl fmt::Display for FileLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.lineno,self.column)
    }
}



include!(concat!(env!("OUT_DIR"), "/keywords.rs"));

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Tok, FileLocation, LexicalError>;
    fn next(&mut self) -> Option<Self::Item> {
        fn IsDigit(ch: char) -> bool {
            match ch {
                '0'..='9' => true,
                _         => false,
            }
        }
        fn IsAlpha(ch: char) -> bool {
            match ch {
                'a'..='z' => true,
                'A'..='Z' => true,
                _         => false,
            }
        }
        let mut word: String = String::new();
        let mut ch: char;
        match self.ReadChar() {
            Err(message) => {return Some(Err(LexicalError::IOError(message.to_string())));},
            Ok(nextChar) => {
                if nextChar.is_none() {return None;}
                ch = nextChar.unwrap();
            },
        };
        eprintln!("*** Lexer::next(): lineno is {}, first_column is {}, current_column is {}",
                self.lineno, self.first_column, self.current_column);
        eprintln!("*** Lexer::next(): ch = '{}'",ch);
        eprintln!("*** Lexer::next(): self.scaneol is {:?}",self.scaneol);
        if self.scaneol == ScanEOL::On {
            while ch != '\n' {
                word.push(ch);
                self.current_column += 1;
                match self.ReadChar() {
                    Err(message) => {return Some(Err(LexicalError::IOError(message.to_string())));},
                    Ok(nextChar) => {
                        if nextChar.is_none() {
                            ch = '\n';
                        } else {
                            ch = nextChar.unwrap();
                        }
                    },
                };
            }
            self.peekChar = Some(ch);
            let first = FileLocation::new(self.lineno,self.first_column);
            let last = FileLocation::new(self.lineno,self.current_column);
            self.first_column = self.current_column + 1;
            self.scaneol = ScanEOL::Off;
            return Some(Ok((first,Tok::STRINGTOEOL(word.clone()),last)));
        } else {
            while ch == ' ' || ch == '\t' {
                self.current_column += 1;
                match self.ReadChar() {
                    Err(message) => {return Some(Err(LexicalError::IOError(message.to_string())));},
                    Ok(nextChar) => {
                        if nextChar.is_none() {
                            ch = '\n';
                        } else {
                            ch = nextChar.unwrap();
                        }
                    },
                };
            }
            if ch == '\n' {
                let first = FileLocation::new(self.lineno,self.first_column);
                let last  = FileLocation::new(self.lineno,self.current_column + 1);
                self.first_column = 0;
                self.current_column = 0;
                self.lineno += 1;
                return Some(Ok((first,Tok::EOL,last)));
            }
            if ch == '#' {
                while ch != '\n' {
                    self.current_column += 1;
                    match self.ReadChar() {
                        Err(message) => {return Some(Err(LexicalError::IOError(message.to_string())));},
                        Ok(nextChar) => {
                            if nextChar.is_none() {
                                ch = '\n';
                            } else {
                                ch = nextChar.unwrap();
                            }
                        },
                    };
                }
                let first = FileLocation::new(self.lineno, self.first_column);
                let last = FileLocation::new(self.lineno,self.current_column + 1);
                self.first_column = 0;
                self.current_column = 0;
                self.lineno += 1;
                return Some(Ok((first,Tok::EOL,last))); 
            }
            let mut peekch: char;
            match self.PeekChar() {
                Err(message) => {return Some(Err(LexicalError::IOError(message.to_string())));},
                Ok(nextChar) => {
                    if nextChar.is_none() {
                        peekch = '\n';
                    } else {
                        peekch = nextChar.unwrap();
                    }
                },
            };
            if IsDigit(ch) ||
                    ((ch == '+' || ch == '-') &&
                        IsDigit(peekch)) {
                word.push(ch);
                self.current_column += 1;
                match self.ReadChar() {
                    Err(message) => {return Some(Err(LexicalError::IOError(message.to_string())));},
                    Ok(nextChar) => {
                        if nextChar.is_none() { 
                            ch = '\n';
                        } else {
                            ch = nextChar.unwrap();
                        }
                    },
                };
                while IsDigit(ch) {
                    word.push(ch);
                    match self.ReadChar() {
                        Err(message) => {return Some(Err(LexicalError::IOError(message.to_string())));},
                        Ok(nextChar) => {
                            if nextChar.is_none() { 
                                ch = '\n';
                            } else {
                                ch = nextChar.unwrap();
                            }
                        },
                    };
                    self.current_column += 1;
                }
                if self.floatenable && ch == '.' {
                    word.push(ch);
                    match self.ReadChar() {
                        Err(message) => {return Some(Err(LexicalError::IOError(message.to_string())));},
                        Ok(nextChar) => {
                            if nextChar.is_none() { 
                                ch = '\n';
                            } else {
                                ch = nextChar.unwrap();
                            }
                        },
                    };
                    self.current_column += 1;
                    while IsDigit(ch) {
                        word.push(ch);
                        match self.ReadChar() {
                            Err(message) => {return Some(Err(LexicalError::IOError(message.to_string())));},
                            Ok(nextChar) => {
                                if nextChar.is_none() { 
                                    ch = '\n';
                                } else {
                                    ch = nextChar.unwrap();
                                }
                            },
                        };
                        self.current_column += 1;
                    }
                    self.peekChar = Some(ch);
                    let f: f64 = f64::from_str(&word).unwrap();
                    let result = Some(Ok((FileLocation::new(self.lineno,self.first_column),
                                          Tok::FLOAT(f),
                                          FileLocation::new(self.lineno,self.current_column+1))));
                    self.first_column = self.current_column+1;
                    return result;
                }
                self.peekChar = Some(ch);
                let ui: u32 = u32::from_str(&word).unwrap();
                let result = Some(Ok((FileLocation::new(self.lineno,self.first_column),
                                      Tok::UINTEGER(ui),
                                      FileLocation::new(self.lineno,self.current_column+1))));
                self.first_column = self.current_column+1;
                if self.scaneol == ScanEOL::Maybe {self.scaneol = ScanEOL::On;}
                return result;
            } else if IsAlpha(ch) {
                while IsAlpha(ch) || ch == '$' {
                    word.push(ch.to_ascii_uppercase());
                    match self.ReadChar() {
                        Err(message) => {return Some(Err(LexicalError::IOError(message.to_string())));},
                        Ok(nextChar) => {
                            if nextChar.is_none() { 
                                ch = '\n';
                            } else {
                                ch = nextChar.unwrap();
                            }
                        },
                    };
                    self.current_column += 1;
                }
                self.peekChar = Some(ch);
                let firstColumn = FileLocation::new(self.lineno,self.first_column);
                let lastColumn = FileLocation::new(self.lineno,self.current_column + 1);
                self.first_column = self.current_column + 1;
                let token = KEYWORDS.get(&word).cloned();
                if token.is_none() {
                    return Some(Err(LexicalError::UnknownKeyword(word)));
                } else {
                    let tok = token.unwrap();
                    match tok {
                        Tok::TITLE => {self.scaneol = ScanEOL::Maybe;},
                        Tok::VERSION => {self.floatenable = false;},
                        _            => {
                            self.scaneol = ScanEOL::Off;
                            self.floatenable = true;
                        },
                    };
                    return Some(Ok((firstColumn,tok,lastColumn)));
                }
            } else if ch == '"' {
                let mut endOfString: bool = false;
                match self.ReadChar() {
                    Err(message) => {return Some(Err(LexicalError::IOError(message.to_string())));},
                    Ok(nextChar) => {
                        if nextChar.is_none() { 
                            ch = '\n';
                        } else {
                            ch = nextChar.unwrap();
                        }
                    },
                };
                self.current_column += 1;
                while !endOfString && ch != '\n' {
                    if ch == '\\' {
                        match self.ReadChar() {
                            Err(message) => {return Some(Err(LexicalError::IOError(message.to_string())));},
                            Ok(nextChar) => {
                                if nextChar.is_none() { 
                                    ch = '\n';
                                } else {
                                    ch = nextChar.unwrap();
                                }
                            },
                        };
                        self.current_column += 1; 
                        word.push(ch);
                        match self.ReadChar() {
                            Err(message) => {return Some(Err(LexicalError::IOError(message.to_string())));},
                            Ok(nextChar) => {
                                if nextChar.is_none() { 
                                    ch = '\n';
                                } else {
                                    ch = nextChar.unwrap();
                                }
                            },
                        };
                    } else if ch == '"' {
                        match self.PeekChar() {
                            Err(message) => {return Some(Err(LexicalError::IOError(message.to_string())));},
                            Ok(nextChar) => {
                                if nextChar.is_none() {
                                    peekch = '\n';
                                } else {
                                    peekch = nextChar.unwrap();
                                }
                            },
                        };
                        if peekch == '"' {
                            ch = peekch;
                            self.peekChar = None;
                            self.current_column += 1;
                            word.push(ch);
                        } else {
                            endOfString = true;
                        }
                    } else if ch != '\n' {
                        word.push(ch);
                        self.current_column += 1;
                        match self.ReadChar() {
                            Err(message) => {return Some(Err(LexicalError::IOError(message.to_string())));},
                            Ok(nextChar) => {
                                if nextChar.is_none() { 
                                    ch = '\n';
                                } else {
                                    ch = nextChar.unwrap();
                                }
                            },
                        };
                    } else {
                        return Some(Err(LexicalError::UnTerminatedString));
                    }    
                }
                self.current_column += 1; 
                let firstCol = FileLocation::new(self.lineno,self.first_column);
                let lastCol = FileLocation::new(self.lineno,self.current_column + 1);
                self.first_column = self.current_column + 1;
                return Some(Ok((firstCol,Tok::STRING(word.clone()),lastCol)));
            } else if ch == '.' {
                self.current_column += 1; 
                let firstCol = FileLocation::new(self.lineno,self.first_column);
                let lastCol = FileLocation::new(self.lineno,self.current_column);
                self.first_column = self.current_column+1;
                return Some(Ok((firstCol,Tok::DOT,lastCol)));
            } else {
                return Some(Err(LexicalError::UnknownCharacter(ch)));
            }
        }
    }
}













impl fmt::Display for LayoutError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LayoutError::ParseError(message) => write!(f, "Parse Error: {}", message),
            LayoutError::FileError(message) => write!(f, "File error: {}", message),
            LayoutError::IOError(message) => write!(f, "IO Error: {}", message),
        }
    }
}


impl Layout {
    pub fn new(layoutfilename: String) -> Result<Self, LayoutError> {
        let mut this = Self{};
        let file = match File::open(&layoutfilename) {
            Ok(f) => f,
            Err(message) => {
                return Err(LayoutError::FileError(message.to_string()));
            },
        };
        let mut reader = BufReader::new(file);
        let lexer = Lexer::new(&mut reader);
        let parser = xtrakcad::XtrakCadLayoutParser::new();
        match parser.parse(&mut this,lexer) {
            Ok(()) => (),
            Err(message) => {
                return Err(LayoutError::ParseError(message.to_string()));
            },
        };
        Ok(this)
    }
    pub fn SetVersion(&mut self,fver: u32, major: u32, minor: u32, release: u32) {
        eprintln!("*** Layout::SetVersion({},{},{},{})",fver,major,minor,release);
    }
    pub fn SetTitle(&mut self,level: u32, text: String) {
        eprintln!("*** Layout::SetTitle({},'{}')",level,text);
    }
    pub fn SetMapscale(&mut self,mapscale: u32) {
    }
    pub fn SetRoomsize(&mut self,width: f64, height: f64) {
    }
    pub fn SetScale(&mut self,scale: Scale) {
    }
    pub fn AddLayer(&mut self,lnum: u32,visible: u32,frozen: u32,on_map: u32,
                    color_rgb: u32,module: u32,dont_use_color: u32,
                    color_flags: u32,button_off: u32,name: String,inherit: u32,
                    scale_index: u32,min_track_radius: f64,
                    max_track_grade: f64,tie_length: f64,tie_width: f64,
                    tie_spacing:f64) {
    }
    pub fn SetCurrentLayer(&mut self,lnum: u32) {
    }
    pub fn AddStructure(&mut self,index: u32, layer: u32, lineType: u32, 
                        pad1: u32, pad2: u32, scale: Scale, visible: u32, 
                        origx: f64, origy: f64, elev: u32, angle: f64, 
                        textfields: String, adjopt: Option<(f64, f64)>, 
                        pieropt: Option<(f64, String)>, 
                        structbody: StructureBody) {
    }
    pub fn AddDrawing(&mut self,index: u32, layer: u32, lineType: u32,
                        pad1: u32, pad2: u32, start_x: f64, start_y: f64,
                        start: u32, angle: f64, segments: StructureBody) {
    }
    pub fn AddBZRLine(&mut self,index: u32, layer: u32, pad1: u32, pad2: u32,
                      line_width: u32, scale: Scale, visible: u32, X1: f64,
                      Y1: f64,X2: f64,Y2: f64,X3: f64,Y3: f64,X4: f64,Y4: f64,
                      pad3: u32,desc_X: f64,desc_Y: f64, body: BZRLineBody) {
    }
    pub fn AddCornu(&mut self,index: u32,layer: u32,width: u32,pad1: u32,
                    pad2: u32,scale: Scale,visible: u32,pos1x: f64,pos1y: f64,
                    angle1: f64,radius1: f64,center1x: f64,center1y: f64,
                    pos2x: f64,pos2y: f64,angle2: f64,radius2: f64,
                    center2x: f64,center2y: f64,body: CornuBody) {
    }
}

pub enum BZSegment {
    S1(u32,f64,f64,f64,f64,f64),
    S2(u32,u32,f64,f64,f64,f64,f64,f64,f64),
    C1(u32,f64,f64,f64,f64,f64,f64),
    C2(u32,u32,f64,f64,f64,f64,f64,f64,f64),
}

#[derive(Debug, PartialEq, Clone)]
pub struct BZLSegments {
}

impl BZLSegments {
    pub fn new() -> Self {
        Self{}
    }
    pub fn Append(e: StructureBodyElement, b:BZLSegments) -> Self {
        b
    }
}
#[derive(Debug, PartialEq, Clone)] 
pub struct FBlockElement(f64,f64,u32);
    
#[derive(Debug, PartialEq, Clone)]
pub struct FBlock {
}

impl FBlock {
    pub fn new() -> Self {
        Self{}
    }
    pub fn Append(e: FBlockElement, b: FBlock) -> Self {
        b
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum StructureBodyElement {
    D(f64,f64),
    L(u32,u32,f64,f64,f64,u32,f64,f64,u32),
    M(u32,u32,f64,f64,f64,u32,f64,f64,u32,u32),
    F(u32,u32,f64,u32,Option<u32>,FBlock),
    A(u32,u32,f64,f64,f64,f64,u32,f64,f64),
    B(u32,u32,f64,f64,f64,u32,f64,f64,u32,u32),
    Q(u32,u32,f64,f64,f64,u32,f64,f64,u32),
    G(u32,u32,f64,f64,f64,f64,u32),
    Y(u32,u32,f64,u32,u32,FBlock),
    Z(u32,f64,f64,f64,u32,f64,String),
    H(u32,u32,f64,f64,f64,f64,f64,f64,f64,f64,BZLSegments),
}

#[derive(Debug)]
pub struct StructureBody {
}

impl StructureBody {
    pub fn new() -> Self {
        Self {}
    }
    pub fn Append(e: StructureBodyElement, b: StructureBody) -> Self {
        b
    }
}

#[derive(Debug)]
pub struct BZRLineBody {
}

impl BZRLineBody {
    pub fn new() -> Self {
        Self{}
    }
    pub fn Append(e: StructureBodyElement, b: BZRLineBody) -> Self {
        b
    }
}

pub struct CornuBodyElement(u32,u32,f64,f64,f64,f64,f64,f64,f64,f64,f64,
                            CornuBody);

pub struct CornuBody {
}

impl CornuBody {
    pub fn new() -> Self {
        Self{}
    }
    pub fn AppendTrack(e: TrackBodyElement, b: CornuBody) -> Self {
        b
    }
    pub fn AppendCornu(e: CornuBodyElement, b: CornuBody) -> Self {
        b
    }
    pub fn AppendBZSeg(e: BZSegment, b: CornuBody) -> Self {
        b
    }
}

pub enum FloatOrString {
    Float(f64),
    String(String),
}

pub enum TrackBodyElement {
    T1(u32,f64,f64,f64,Option<TrackBodySubElement>),
    T4(u32,u32,f64,f64,f64,TrackBodySubElement),
    E1(f64,f64,f64,Option<TrackBodySubElement>),
    E4(u32,f64,f64,f64,TrackBodySubElement),
}

pub enum TrackBodySubElement {
    T1(u32,f64,f64,Option<FloatOrString>),
    T4(u32,f64,f64,FloatOrString,f64,u32,u32,u32,f64),
}

pub struct TrackBody {
}

impl TrackBody {
    pub fn new() -> Self {
        Self{}
    }
    pub fn AppendTrack(e:TrackBodyElement, b: TrackBody) -> Self {
        b
    }
}
