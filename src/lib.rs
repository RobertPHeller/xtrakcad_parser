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
            Tok::EOL => write!(f,"\\n" ),
            Tok::DOT => write!(f,"." ),
            Tok::STRINGTOEOL(s) => write!(f,"Tok::STRINGTOEOL({})",s ),
            Tok::NULL => write!(f,"Tok::NULL" ),
            Tok::STRING(s) => write!(f,"STRING({})", s),
            Tok::UINTEGER(u) => write!(f,"UINTEGER({})",u ),
            Tok::FLOAT(flo) => write!(f,"FLOAT({})", flo ),
            Tok::A => write!(f,"A" ),
            Tok::ADJUSTABLE => write!(f,"ADJUSTABLE" ),
            Tok::ASPECT => write!(f,"ASPECT" ),
            Tok::B => write!(f,"B" ),
            Tok::BEZIER => write!(f,"BEZIER" ),
            Tok::BLOCK => write!(f,"BLOCK" ),
            Tok::BZRLIN => write!(f,"BZRLIN" ),
            Tok::C => write!(f,"C" ),
            Tok::CAR => write!(f,"CAR" ),
            Tok::CONTROL => write!(f,"CONTROL" ),
            Tok::CORNU => write!(f,"CORNU" ),
            Tok::CURRENT => write!(f,"CURRENT" ),
            Tok::CURVE => write!(f,"CURVE" ),
            Tok::D => write!(f,"D" ),
            Tok::DRAW => write!(f,"DRAW" ),
            Tok::E => write!(f,"E" ),
            Tok::ENDBLOCK => write!(f,"ENDBLOCK" ),
            Tok::ENDSEGS => write!(f,"ENDSEGS" ),
            Tok::ENDSIGNAL => write!(f,"ENDSIGNAL" ),
            Tok::ENDTRACKS => write!(f,"ENDTRACKS" ),
            Tok::F => write!(f,"F" ),
            Tok::G => write!(f,"G" ),
            Tok::H => write!(f,"H" ),
            Tok::HO => write!(f,"HO" ),
            Tok::J => write!(f,"J" ),
            Tok::JOINT => write!(f,"JOINT" ),
            Tok::L => write!(f,"L" ),
            Tok::LAYERS => write!(f,"LAYERS" ),
            Tok::M => write!(f,"M" ),
            Tok::MAIN => write!(f,"MAIN" ),
            Tok::MAPSCALE => write!(f,"MAPSCALE" ),
            Tok::N => write!(f,"N" ),
            Tok::NOTE => write!(f,"NOTE" ),
            Tok::O => write!(f,"O" ),
            Tok::P => write!(f,"P" ),
            Tok::PIER => write!(f,"PIER" ),
            Tok::Q => write!(f,"Q" ),
            Tok::ROOMSIZE => write!(f,"ROOMSIZE" ),
            Tok::S => write!(f,"S" ),
            Tok::SCALE => write!(f,"SCALE" ),
            Tok::SENSOR => write!(f,"SENSOR" ),
            Tok::SIGNAL => write!(f,"SIGNAL" ),
            Tok::STRAIGHT => write!(f,"STRAIGHT" ),
            Tok::STRUCTURE => write!(f,"STRUCTURE" ),
            Tok::SUBSEGS => write!(f,"SUBSEGS" ),
            Tok::SUBSEND => write!(f,"SUBSEND" ),
            Tok::SWITCHMOTOR => write!(f,"SWITCHMOTOR" ),
            Tok::T => write!(f,"T" ),
            Tok::TEXT => write!(f,"TEXT" ),
            Tok::TITLE => write!(f,"TITLE" ),
            Tok::TRK => write!(f,"TRK" ),
            Tok::TURNOUT => write!(f,"TURNOUT" ),
            Tok::TURNTABLE => write!(f,"TURNTABLE" ),
            Tok::VERSION => write!(f,"VERSION" ),
            Tok::W => write!(f,"W" ),
            Tok::X => write!(f,"X" ),
            Tok::Y => write!(f,"Y" ),
            Tok::Z => write!(f,"Z" ),
        }
    }
}

impl Tok {
    pub fn StringValue(self) -> String {
        match self {
            Tok::STRING(s) |
            Tok::STRINGTOEOL(s) => s,
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
        write!(f, "Line:{}/Col:{}", self.lineno,self.column)
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
        //eprintln!("*** Lexer::next(): lineno is {}, first_column is {}, current_column is {}",
        //        self.lineno, self.first_column, self.current_column);
        //eprintln!("*** Lexer::next(): ch = '{}'",ch);
        //eprintln!("*** Lexer::next(): self.scaneol is {:?}",self.scaneol);
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
        eprintln!("*** Layout::SetMapscale({})",mapscale);
    }
    pub fn SetRoomsize(&mut self,width: f64, height: f64) {
        eprintln!("*** Layout::SetRoomsize({},{})",width,height);
    }
    pub fn SetScale(&mut self,scale: Scale) {
        eprintln!("*** Layout::SetScale({:?})",scale);
    }
    pub fn AddLayer(&mut self,lnum: u32,visible: u32,frozen: u32,on_map: u32,
                    color_rgb: u32,module: u32,dont_use_color: u32,
                    color_flags: u32,button_off: u32,name: String,inherit: u32,
                    scale_index: u32,min_track_radius: f64,
                    max_track_grade: f64,tie_length: f64,tie_width: f64,
                    tie_spacing:f64) {
        eprintln!("*** Layout::AddLayer({},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{})",lnum,visible,frozen,on_map,color_rgb,module,dont_use_color,color_flags,button_off,name,inherit,scale_index,min_track_radius,max_track_grade,tie_length,tie_width,tie_spacing);
    }
    pub fn SetCurrentLayer(&mut self,lnum: u32) {
        eprintln!("*** Layout::SetCurrentLayer({})",lnum);
    }
    pub fn AddStructure(&mut self,index: u32, layer: u32, lineType: u32, 
                        pad1: u32, pad2: u32, scale: Scale, visible: u32, 
                        origx: f64, origy: f64, elev: u32, angle: f64, 
                        textfields: String, adjopt: Option<(f64, f64)>, 
                        pieropt: Option<(f64, String)>, 
                        structbody: StructureBody) {
        eprintln!("*** Layout::AddStructure({},{},{},{},{},{:?},{},{},{},{},{},{},{:?},{:?},{:?})",index,layer,lineType,pad1,pad2,scale,visible,origx,origy,elev,angle,textfields,adjopt,pieropt,structbody);
    }
    pub fn AddDrawing(&mut self,index: u32, layer: u32, lineType: u32,
                        pad1: u32, pad2: u32, start_x: f64, start_y: f64,
                        start: u32, angle: f64, segments: StructureBody) {
        eprintln!("*** Layout::AddDrawing({},{},{},{},{},{},{},{},{},{:?})",index,layer,lineType,pad1,pad2,start_x,start_y,start,angle,segments);
    }
    pub fn AddBZRLine(&mut self,index: u32, layer: u32, pad1: u32, pad2: u32,
                      line_width: u32, scale: Scale, visible: u32, X1: f64,
                      Y1: f64,X2: f64,Y2: f64,X3: f64,Y3: f64,X4: f64,Y4: f64,
                      pad3: u32,desc_X: f64,desc_Y: f64, body: BZRLineBody) {
        eprintln!("*** Layout::AddBZRLine({},{},{},{},{},{:?},{},{},{},{},{},{},{},{},{},{},{},{},{:?})",
                    index,layer,pad1,pad2,line_width,scale,visible,X1,Y1,X2,Y2,
                    X3,Y3,X4,Y4,pad3,desc_X,desc_Y,body);
    }
    pub fn AddCornu(&mut self,index: u32,layer: u32,width: u32,pad1: u32,
                    pad2: u32,scale: Scale,visible: u32,pos1x: f64,pos1y: f64,
                    angle1: f64,radius1: f64,center1x: f64,center1y: f64,
                    pos2x: f64,pos2y: f64,angle2: f64,radius2: f64,
                    center2x: f64,center2y: f64,body: CornuBody) {
        eprintln!("*** Layout::AddCornu({},{},{},{},{},{:?},{},{},{},{},{},{},{},{},{},{},{},{},{},{:?})",
                    index,layer,width,pad1,pad2,scale,visible,pos1x,pos1y,
                    angle1,radius1,center1x,center1y,pos2x,pos2y,angle2,
                    radius2,center2x,center2y,body);
    }
    pub fn AddCurve(&mut self,index: u32, layer: u32, line_width: u32, 
                    pad1: u32, pad2: u32, scale: Scale, flags: u32, 
                    center_X: f64, centerY: f64, pad3: u32, radius: f64, 
                    helix_turns: u32, desc_X: f64, desc_Y: f64, 
                    trackbody: TrackBody) {
        eprintln!("*** Layout::AddCurve({},{},{},{},{},{:?},{},{},{},{},{},{},{},{},{:?})",
                    index,layer,pad1,pad2,line_width,scale,flags,center_X,
                    centerY,pad3,radius,helix_turns,desc_X,desc_Y,trackbody);
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
    elements: Vec<StructureBodyElement>,}

impl BZLSegments {
    pub fn new() -> Self {
        Self{elements: Vec::new()}
    }
    pub fn Append(e: StructureBodyElement, mut b:BZLSegments) -> Self {
        b.elements.insert(0,e);
        b
    }
}
#[derive(Debug, PartialEq, Clone)] 
pub struct FBlockElement(f64,f64,u32);
    
#[derive(Debug, PartialEq, Clone)]
pub struct FBlock {
    elememts: Vec<FBlockElement>
}

impl FBlock {
    pub fn new() -> Self {
        eprintln!("*** FBlock::new()");
        Self{ elememts: Vec::new()}
    }
    pub fn Append(e: FBlockElement, mut b: FBlock) -> Self {
        eprintln!("*** FBlock::Append({:?},{:?})",e,b);
        b.elememts.insert(0,e);
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
    elememts: Vec<StructureBodyElement>,
}

impl StructureBody {
    pub fn new() -> Self {
        eprintln!("*** StructureBody::new()");
        Self {elememts: Vec::new()}
    }
    pub fn Append(e: StructureBodyElement, mut b: StructureBody) -> Self {
        eprintln!("*** StructureBody::Append({:?},{:?})",e,b);
        b.elememts.insert(0,e);
        b
    }
}

#[derive(Debug)]
pub struct BZRLineBody {
    elements: Vec<StructureBodyElement>,
}

impl BZRLineBody {
    pub fn new() -> Self {
        Self{ elements: Vec::new()}
    }
    pub fn Append(e: StructureBodyElement, mut b: BZRLineBody) -> Self {
        b.elements.insert(0,e);
        b
    }
}

#[derive(Debug)]
pub struct CornuBodyElement(u32,u32,f64,f64,f64,f64,f64,f64,f64,f64,f64,
                            CornuBody);

#[derive(Debug)]
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

#[derive(Debug)]
pub enum FloatOrString {
    Float(f64),
    String(String),
}

#[derive(Debug)]
pub enum TrackBodyElement {
    T1(u32,f64,f64,f64,Option<TrackBodySubElement>),
    T4(u32,u32,f64,f64,f64,TrackBodySubElement),
    E1(f64,f64,f64,Option<TrackBodySubElement>),
    E4(u32,f64,f64,f64,TrackBodySubElement),
}

#[derive(Debug)]
pub enum TrackBodySubElement {
    T1(u32,f64,f64,Option<FloatOrString>),
    T4(u32,f64,f64,FloatOrString,f64,u32,u32,u32,f64),
}

#[derive(Debug)]
pub struct TrackBody {
    elements: Vec<TrackBodyElement>,
}

impl TrackBody {
    pub fn new() -> Self {
        Self{ elements: Vec::new(),}
    }
    pub fn AppendTrack(e:TrackBodyElement, mut b: TrackBody) -> Self {
        b.elements.insert(0,e);
        b
    }
}
