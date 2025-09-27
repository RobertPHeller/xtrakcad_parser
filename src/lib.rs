// -!- rust -!- //////////////////////////////////////////////////////////////
//
//  System        : 
//  Module        : 
//  Object Name   : $RCSfile$
//  Revision      : $Revision$
//  Date          : $Date$
//  Author        : $Author$
//  Created By    : Robert Heller
//  Created       : 2025-09-24 14:45:20
//  Last Modified : <250927.1112>
//
//  Description	
//
//  Notes
//
//  History
//	
/////////////////////////////////////////////////////////////////////////////
//    Copyright (C) 2025  Robert Heller D/B/A Deepwoods Software
//			51 Locke Hill Road
//			Wendell, MA 01379-9728
//
//    This program is free software; you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation; either version 2 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, write to the Free Software
//    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
// 
//
//////////////////////////////////////////////////////////////////////////////

//! XtrkCAD layout file parser in Rust, using a lalrpop parser.
//! Ported from the Bison++/C++ parser that is part of the Model Railroad 
//! System



use lalrpop_util::lalrpop_mod;
use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufReader};
use std::str::FromStr;
use std::fmt;
use std::collections::HashMap;
use std::ops::Index;

// Pull in the parser module
lalrpop_mod!(pub xtrakcad); // synthesized by LALRPOP


/// Lexer result type
pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

/// Lexer tokens
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
    /// Display a Tok enum
    /// ## Parameters:
    /// - f formatter to write to
    ///
    /// __Returns__ a fmt::Result
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
    /// Extract a String value from a token
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the String value
    pub fn StringValue(self) -> String {
        match self {
            Tok::STRING(s) |
            Tok::STRINGTOEOL(s) => s,
            _        => String::new(),
        }
    }
    /// Extract a u32 value from a token
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the u32 value
    pub fn U32Value(self) -> u32 {
        match self {
            Tok::UINTEGER(u) => u,
            _           => 0,
        }
    }
    /// Extract a f64 value from a token
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the f64 value
    pub fn F64Value(self) -> f64 {
        match self {
            Tok::FLOAT(f) => f,
            _        => 0.0,
        }
    }
}

/// Lexer errors
#[derive(Debug, PartialEq, Clone)] 
pub enum LexicalError {
    UnTerminatedString,
    UnknownKeyword(String),
    IOError(String),
    UnknownCharacter(char),
}

impl fmt::Display for LexicalError {
    /// Display a LexicalError enum
    /// ## Parameters:
    /// - f formatter to write to
    ///
    /// __Returns__ a fmt::Result
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

/// Manage special case of parsing Title lines.
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ScanEOL {
    Off,
    Maybe,
    On,
}

/// Lexer structure 
/// Reads characters from the layout file and returns Tokens to the Parser.
/// Handles all of the low-level character level processing
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
    /// Create a new initialized instance of the lexer.
    /// The Lexer struct maintains layout file context throughout the
    /// parsing process, including keep track of the line and column.
    /// It also maintains a one character look ahead.
    /// ## Parameters:
    /// - reader a BufReader instance for the layout file
    ///
    /// __Returns__ a lexer instance that can be passed to the parser.
    pub fn new(reader: &'input mut BufReader<File>) -> Self {
        Lexer { reader: reader, lineno: 1, first_column: 0, 
                current_column: 0, peekChar: None, scaneol: ScanEOL::Off, 
                floatenable: true }
    }
    /// Low level character look ahead.  If the peek buffer is empty, this
    /// method reads one character from the file and saves it in the peek 
    /// buffer.
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the peek buffer.
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
    /// Low level character reader.  This function empties the peek buffer,
    /// returning the next available character.
    /// ## Parameters: 
    /// None 
    /// __Returns__ the next available character or None on EOF
    fn ReadChar(&mut self) -> io::Result<Option<char>> {
        if self.peekChar.is_none() {self.PeekChar()?;}
        let result = self.peekChar;
        self.peekChar = None;
        Ok(result)
    }
}
/// File location structure.  The file's location is a column on a line
#[derive(Debug, PartialEq, Copy, Clone, Default)]
pub struct FileLocation {
    lineno: u32,
    column: usize,
}


impl FileLocation {
    /// Initializer for a file location
    /// ## Parameters:
    /// - l the line number
    /// - c the column
    ///
    /// __Returns__ an initialized FileLocation
    pub fn new(l: u32, c: usize) -> Self {
        Self {lineno: l, column: c }
    }
}

impl fmt::Display for FileLocation {
    /// Display function for a file location.
    /// ## Parameters:
    /// - f a fmt::Formatter object.
    ///
    /// __Returns__ a fmt::Result
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Line:{}/Col:{}", self.lineno,self.column)
    }
}



include!(concat!(env!("OUT_DIR"), "/keywords.rs"));

impl<'input> Iterator for Lexer<'input> {
    /// Iterator resuly type
    type Item = Spanned<Tok, FileLocation, LexicalError>;
    /// Iterator for Lexer: return the next Token
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ The next available token, a lexical error or None on EOF.
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
                        //eprintln!("*** Lexer::next(): in string, ch = '{}'",ch);
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
                        //eprintln!("*** Lexer::next(): in string, peekch = '{}'",peekch);
                        if peekch == '"' {
                            ch = peekch;
                            self.peekChar = None;
                            self.current_column += 1;
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


/// Layout error codes
#[derive(Debug, PartialEq, Clone)]
pub enum LayoutError {
    ParseError(String),
    FileError(String),
    IOError(String),
}


impl fmt::Display for LayoutError {
    /// Display function of layout error codes.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LayoutError::ParseError(message) => write!(f, "Parse Error: {}", message),
            LayoutError::FileError(message) => write!(f, "File error: {}", message),
            LayoutError::IOError(message) => write!(f, "IO Error: {}", message),
        }
    }
}

/// Layer struct
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Layer {
    visible: bool,
    frozen: bool,
    on_map: bool,
    color_rgb: u32,
    module: u32,
    dont_use_color: bool,
    color_flags: u32,
    button_off: bool,
    name: String,
    inherit: bool,
    scale_index: u32,
    min_track_radius: f64,
    max_track_grade: f64,
    tie_length: f64,
    tie_width: f64,
    tie_spacing: f64,
}

impl fmt::Display for Layer {
    /// Display a Layer
    /// ## Parameters:
    /// - f formatter to write to
    ///
    /// __Returns__ a fmt::Result
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<#Layer {}>", self.name)
    }
}

impl Layer {
    /// Initialize a layer
    /// ## Parameters:
    /// - visible is it visible?
    /// - frozen is it frozen?
    /// - on_map is it on map?
    /// - color_rgb its colot
    /// - module its module
    /// - dont_use_color don't use color?
    /// - color_flags color flags
    /// - button_off is it button off?
    /// - name its name
    /// - inherit inherit?
    /// - scale_index its scale index
    /// - min_track_radius minimum track radious
    /// - max_track_grade maximum grade
    /// - tie_length tie length
    /// - tie_width tie width
    /// - tie_spacing tie spacing
    ///
    /// __Returns__ an initialized Layer struct.
    pub fn new(visible: bool,frozen: bool,on_map: bool,color_rgb: u32,
               module: u32,dont_use_color: bool,color_flags: u32,
               button_off: bool,name: String,inherit: bool,scale_index: u32,
               min_track_radius: f64,max_track_grade: f64,tie_length: f64,
               tie_width: f64,tie_spacing: f64) -> Self {
        Self {visible: visible,frozen: frozen,on_map: on_map,color_rgb: 
              color_rgb,module: module,dont_use_color: dont_use_color,
              color_flags: color_flags,button_off: button_off,name: name,
              inherit: inherit,scale_index: scale_index,
              min_track_radius: min_track_radius,
              max_track_grade: max_track_grade,tie_length: tie_length,
              tie_width: tie_width,tie_spacing: tie_spacing}
    }
    /// Is the layer visible?
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ Visiblity flag
    pub fn IsVisibleP(&self) -> bool {self.visible}
    /// Is frozen?
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ frozen flag
    pub fn IsFrozenP(&self) -> bool {self.frozen}
    /// Is On Map?
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ on map flag
    pub fn IsOnMapP(&self) -> bool {self.on_map}
    /// Layer color
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ layer's color
    pub fn Color(&self) -> u32 {self.color_rgb}
    /// Module
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ layer's module
    pub fn Module(&self) -> u32 {self.module}
    /// Don't use color?
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ layer's Don't use color flag
    pub fn DontUseColotP(&self) -> bool {self.dont_use_color}
    /// Color Flags
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ layer's Color Flags
    pub fn ColorFlags(&self) -> u32 {self.color_flags}
    /// Is button off?
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ Is layer's button off?
    pub fn IsButtonOffP(&self) -> bool {self.button_off}
    /// Layer name
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ layer's name
    pub fn Name(&self) -> String {self.name.clone()}
    /// Inherit?
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layer's inherit flag
    pub fn InheritP(&self) -> bool {self.inherit}
    /// Scale Index
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ The layer's scale index
    pub fn ScaleIndex(&self) -> u32 {self.scale_index}
    /// Minimum track radius
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layer's minimum track radius
    pub fn MinimumTrackRadius(&self) -> f64 {self.min_track_radius}
    /// Maximum Track Grade
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layer's maximum track grade 
    pub fn MaximumTrackGrade(&self) -> f64 {self.max_track_grade}
    /// Tie Length
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layer's tie length
    pub fn TieLength(&self) -> f64 {self.tie_length}
    /// Tie Width
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layer's tie width
    pub fn TieWidth(&self) -> f64 {self.tie_width}
    /// Tie spacing
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layer's tie spacing
    pub fn TieSPacing(&self) -> f64 {self.tie_spacing}

}

/// Structure struct
#[derive(Debug, Clone, PartialEq)] 
pub struct Structure {
    layer: u32,
    lineType: u32,
    scale: Scale,
    visible: bool,
    origx: f64,
    origy: f64,
    elev: u32,
    angle: f64,
    textfields: String,
    adjopt: Option<(f64, f64)>,
    pieropt: Option<(f64, String)>,
    structbody: StructureBody,
}

impl fmt::Display for Structure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut tabs = self.textfields.split('\t');
        let manu = tabs.next().unwrap_or("");
        let name = tabs.next().unwrap_or("");
        let partno = tabs.next().unwrap_or("");
        
        write!(f, "<#Structure ({},{},{})>", manu,name,partno)
    }
}
impl Structure {
    /// Initialize a Structure element
    /// ## Parameters:
    /// - layer The layer the Structure is on
    /// - lineType The line type: 0=solid, 1=dashed, 2=dots, 3=dash-dot, 4=dash-dot-dot
    /// - scale The model scale
    /// - visible Is it visible
    /// - origx The X origin
    /// - origy The Y origin
    /// - elev The elevation
    /// - angle The angle
    /// - textfields The text fields
    /// - adjopt The optional adjustments
    /// - pieropt The optional Peirs
    /// - structbody The body of the structure
    ///
    /// __Returns__ an initialized Structure
    pub fn new(layer: u32, lineType: u32, scale: Scale, visible: bool, 
               origx: f64, origy: f64, elev: u32, angle: f64, 
               textfields: String, adjopt: Option<(f64, f64)>, 
               pieropt: Option<(f64, String)>, structbody: StructureBody) 
                -> Self {
         Self { layer: layer, lineType: lineType, scale: scale, 
               visible: visible, origx: origx, origy: origy, elev: elev, 
               angle: angle, textfields: textfields, adjopt: adjopt, 
               pieropt: pieropt, structbody: structbody }
    }
    /// The layer
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layer index
    pub fn Layer(&self) -> u32 {self.layer}
    /// The line type
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the line type
    pub fn LineType(&self) -> u32 {self.lineType}
    /// The model scale
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the model scale
    pub fn Scale(&self) -> Scale {self.scale}
    /// Is the structure visible?
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the the structure's visibility
    pub fn IsVisibleP(&self) -> bool {self.visible}
    /// The X origin
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the X origin
    pub fn XOrigin(&self) -> f64 {self.origx}
    /// The Y origin
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the Y origin
    pub fn YOrigin(&self) -> f64 {self.origy}
    /// The elevation
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the Elevation
    pub fn Elevation(&self) -> u32 {self.elev}
    /// The angle
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the angle
    pub fn Angle(&self) -> f64 {self.angle}
    /// The textfields
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the text fields
    pub fn TextFields(&self) -> String {self.textfields.clone()}
    /// The optional Adjustable Options
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the optional Adjustable Options
    pub fn AdjustableOptions(&self) -> Option<(f64, f64)> {self.adjopt}
    /// The optional pier options
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the optional Pier Options
    pub fn PeirOptions(&self) -> Option<(f64, String)> {self.pieropt.clone()}
    /// The structure body
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the structure body
    pub fn StructureBody(&self) -> StructureBody {self.structbody.clone()}
}

/// Drawing struct
#[derive(Debug, Clone, PartialEq)]
pub struct Drawing {
    layer: u32, 
    lineType: u32,
    start_x: f64, 
    start_y: f64, 
    start: u32, 
    angle: f64, 
    segments: StructureBody,
}

impl fmt::Display for Drawing {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<#Drawing {},{},{} {} segments>",
                        self.start_x,self.start_y,self.angle,
                        self.segments.len())
    }
}

impl Drawing {
    /// Initialize a new Drawing struct
    /// ## Parameters:
    /// - layer The layer the drawing is on
    /// - lineType The line type 
    /// - start_x Starting X
    /// - start_y Starting Y 
    /// - start Start?  
    /// - angle Angle
    /// - segments body segments 
    ///
    /// __Returns__ an initialized Drawing struct
    pub fn new(layer: u32, lineType: u32, start_x: f64, start_y: f64, 
                start: u32, angle: f64, segments: StructureBody) -> Self {
        Self {layer: layer, lineType: lineType, start_x: start_x, 
              start_y: start_y, start: start, angle: angle, segments: segments}
    }
    /// The layer
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layer index
    pub fn Layer(&self) -> u32 {self.layer}
    /// The Line Type
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the Line Type 
    pub fn LineType(&self) -> u32 {self.lineType}
    /// The Start X
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the Start X
    pub fn StartX(&self) -> f64 {self.start_x}
    /// The Start Y
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the Start Y
    pub fn StartY(&self) -> f64 {self.start_y}
    /// The Start
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the Start
    pub fn Start(&self) -> u32 {self.start}
    /// The Angle
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the Angle
    pub fn Angle(&self) -> f64 {self.angle}
    /// The Segments
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the Segments
    pub fn Segments(&self) -> StructureBody {self.segments.clone()}

}

/// BZRLine struct
#[derive(Debug, Clone, PartialEq)]
pub struct BZRLine {
    layer: u32,
    line_width: u32, 
    scale: Scale, 
    visible: bool,
    x1: f64,
    y1: f64,
    x2: f64,
    y2: f64,
    x3: f64,
    y3: f64,
    x4: f64,
    y4: f64,
    desc_x: f64,
    desc_y: f64,
    body: BZRLineBody,
}

impl fmt::Display for BZRLine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<#BZRLine {} {} {} {} {} {} {} {}>", self.x1, self.y1,
                self.x2,self.y2,self.x3,self.y3,self.x4,self.y4)
    }
}

impl BZRLine {
    /// Initialize a BZRLine struct
    /// ## Parameters:
    /// - layer the layer the line is on
    /// - line_width line width
    /// - scale the model scale
    /// - visible is it visible
    /// - X1 first X coord
    /// - Y1 first Y coord
    /// - X2 second X coord
    /// - Y2 second Y coord
    /// - X3 third X coord
    /// - Y3 third Y coord
    /// - X4 fourth X coord
    /// - Y4 fourth Y coord
    /// - desc_X description X
    /// - desc_Y description Y
    /// - body body elements
    /// 
    /// __Returns__ an initialized BZRLine struct
    pub fn new(layer: u32, line_width: u32, scale: Scale, visible: bool,
               X1: f64, Y1: f64, X2: f64, Y2: f64, X3: f64, Y3: f64,
               X4: f64, Y4: f64, desc_X: f64, desc_Y: f64, body: BZRLineBody)
            -> Self {
        Self {layer: layer, line_width: line_width, scale: scale, 
              visible: visible, x1: X1, y1: Y1, x2: X2, y2: Y2, x3: X3, y3: Y3,
              x4: X4, y4: Y4, desc_x: desc_X, desc_y: desc_Y, body: body}
    }
    /// The layer the line is on
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layer the line is on
    pub fn Layer(&self) -> u32 {self.layer}
    /// The line width
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the line width
    pub fn LineWidth(&self) -> u32 {self.line_width}
    /// The Model scale
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the model Scale
    pub fn Scale(&self) -> Scale {self.scale}
    /// Is the line visible
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the line's visibility
    pub fn IsVisibleP(&self) -> bool {self.visible}
    /// The first X coordinate
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the first X coordinate
    pub fn X1(&self) -> f64 {self.x1}
    /// The first Y coordinate
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the first Y coordinate 
    pub fn Y1(&self) -> f64 {self.y1}
    /// The second X coordinate
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the second X coordinate 
    pub fn X2(&self) -> f64 {self.x2}
    /// The second Y coordinate 
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the second Y coordinate 
    pub fn Y2(&self) -> f64 {self.y2}
    /// The third X coordinate
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the third X coordinate
    pub fn X3(&self) -> f64 {self.x3}
    /// The third Y coordinate
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the thirs Y coordinate
    pub fn Y3(&self) -> f64 {self.y3}
    /// The fourth X coordinate
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the fourth X coordinate
    pub fn X4(&self) -> f64 {self.x4}
    /// The fourth Y coordinate
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the fourth Y coordinate
    pub fn Y4(&self) -> f64 {self.y4}
    /// The X description
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the X description
    pub fn DescX(&self) -> f64 {self.desc_x}
    /// The Y description
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the Y description
    pub fn DescY(&self) -> f64 {self.desc_y}
    /// The body elements
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the body elements
    pub fn Body(&self) -> BZRLineBody {self.body.clone()}
}

/// Cornu curve track struct
#[derive(Debug, Clone, PartialEq)]
pub struct Cornu {
    layer: u32,
    width: u32,
    scale: Scale,
    visible: bool,
    pos1x: f64,
    pos1y: f64,
    angle1: f64,
    radius1: f64,
    center1x: f64,
    center1y: f64,
    pos2x: f64,
    pos2y: f64,
    angle2: f64,
    radius2: f64,
    center2x: f64,
    center2y: f64,
    body: CornuBody,
}

impl fmt::Display for Cornu {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<#Cornu {},{},{},{},{},{} {},{},{},{},{},{}>",
                    self.pos1x,self.pos1y,self.angle1,self.radius1,self.center1x,self.center1y,
                    self.pos2x,self.pos2y,self.angle2,self.radius2,self.center2x,self.center2y)
    }
}

impl Cornu {
    /// Initialize a Cornu struct
    /// ## Parameters:
    /// - layer the layer number this object is on
    /// - width the width
    /// - scale the model scale
    /// - visible Is it visible?
    /// - pos1x position 1 x
    /// - pos1y position 1 y
    /// - angle1 position 1 angle
    /// - radius1 position 1 radius
    /// - center1x position 1 center x
    /// - center1y position 1 center y
    /// - pos2x position 2 x
    /// - pos2y position 2 y
    /// - angle2 position 2 angle
    /// - radius2 position 2 radius
    /// - center2x position 2 center x
    /// - center2y position 2 center y
    /// - body the body elements
    ///
    /// __Returns__ an initialized Cornu struct
    pub fn new(layer: u32, width: u32, scale: Scale, visible: bool, pos1x: f64,
               pos1y: f64, angle1: f64, radius1: f64, center1x: f64,
               center1y: f64, pos2x: f64, pos2y: f64, angle2: f64,
               radius2: f64, center2x: f64, center2y: f64, body: CornuBody) -> Self {
        Self {layer: layer, width: width, scale: scale, visible: visible, 
              pos1x: pos1x, pos1y: pos1y, angle1: angle1, radius1: radius1, 
              center1x: center1x, center1y: center1y, pos2x: pos2x, 
              pos2y: pos2y, angle2: angle2, radius2: radius2, 
              center2x: center2x, center2y: center2y, body: body}
    }
    pub fn Layer(&self) -> u32 {self.layer}
    pub fn Width(&self) -> u32 {self.width}
    pub fn Scale(&self) -> Scale {self.scale}
    pub fn IsVisibleP(&self) -> bool {self.visible}
    pub fn Pos1x(&self) -> f64 {self.pos1x}
    pub fn Pos1y(&self) -> f64 {self.pos1y}
    pub fn Angle1(&self) -> f64 {self.angle1}
    pub fn Radius1(&self) -> f64 {self.radius1}
    pub fn Center1x(&self) -> f64 {self.center1x}
    pub fn Center1y(&self) -> f64 {self.center1y}
    pub fn Pos2x(&self) -> f64 {self.pos2x}
    pub fn Pos2y(&self) -> f64 {self.pos2y}
    pub fn Angle2(&self) -> f64 {self.angle2}
    pub fn Radius2(&self) -> f64 {self.radius2}
    pub fn Center2x(&self) -> f64 {self.center2x}
    pub fn Center2y(&self) -> f64 {self.center2y}
    pub fn Body(&self) -> CornuBody {self.body.clone()}
}

/// Curve track struct
#[derive(Debug, Clone, PartialEq)]
pub struct Curve {
    layer: u32,
    line_width: u32, 
    scale: Scale, 
    flags: u32, 
    center_x: f64, 
    center_y: f64, 
    radius: f64, 
    helix_turns: u32, 
    desc_x: f64, 
    desc_y: f64, 
    trackbody: TrackBody, 
}

impl fmt::Display for Curve {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<#Curve {},{},{} {} turns {},{}>",self.center_x,
                    self.center_y,self.radius,self.helix_turns,self.desc_x,
                    self.desc_y)
    }
}

impl Curve {
    /// Initialize a Curve struct
    /// ## Parameters:
    /// - layer the layer the curve is on
    /// - line_width the line width
    /// - scale the model scale
    /// - flags the flage
    /// - center_X the center x
    /// - center_Y the center y
    /// - radius the radiu s
    /// - helix_turns the number of helix turns
    /// - desc_X the descr x
    /// - desc_Y the descr y
    /// - trackbody the track end points
    ///
    /// __Returns__ an initialized Curve struct
    pub fn new(layer: u32, line_width: u32, scale: Scale, flags: u32, 
               center_X: f64, center_Y: f64, radius: f64, helix_turns: u32, 
               desc_X: f64, desc_Y: f64, trackbody: TrackBody) -> Self {
        Self {layer: layer, line_width: line_width, scale: scale, 
              flags: flags, center_x: center_X, center_y: center_Y, 
              radius: radius, helix_turns: helix_turns, desc_x: desc_X, 
              desc_y: desc_Y, trackbody: trackbody}
    }
    pub fn Layer(&self) -> u32 {self.layer}
    pub fn LineWidth(&self) -> u32 {self.line_width}
    pub fn Scale(&self) -> Scale {self.scale}
    pub fn Flags(&self) -> u32 {self.flags}
    pub fn CenterX(&self) -> f64 {self.center_x}
    pub fn CenterY(&self) -> f64 {self.center_y}
    pub fn Radius(&self) -> f64 {self.radius}
    pub fn HelixTurns(&self) -> u32 {self.helix_turns}
    pub fn DescX(&self) -> f64 {self.desc_x}
    pub fn DescY(&self) -> f64 {self.desc_y}
    pub fn Trackbody(&self) -> TrackBody {self.trackbody.clone()}
}

/// Bezier curve track struct 
#[derive(Debug, Clone, PartialEq)]
pub struct Bezier {
    layer: u32, 
    width: u32, 
    color: u32, 
    scale: Scale, 
    visible: bool, 
    x1: f64, 
    y1: f64, 
    x2: f64, 
    y2: f64, 
    x3: f64, 
    y3: f64, 
    x4: f64, 
    y4: f64, 
    desc_x: f64, 
    desc_y: f64, 
    body: BezierBody, 
}

impl fmt::Display for Bezier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<#Bezier {} {} {} {} {} {} {} {}>", self.x1, self.y1,       
                self.x2,self.y2,self.x3,self.y3,self.x4,self.y4)
    }
}

impl Bezier {
    /// Initialize a Bezier struct
    /// ## Parameters:
    /// - layer The layer the bezier track is on
    /// - width the width
    /// - color the color
    /// - scale the model scale
    /// - visible is it visible
    /// - X1 the first X coordinate
    /// - Y1 the first Y coordinate
    /// - X2 the second X coordinate
    /// - Y2 the second Y coordinate
    /// - X3 the third X coordinate
    /// - Y3 the third Y coordinate
    /// - X4 the fourth X coordinate
    /// - Y4 the fourth Y coordinate
    /// - desc_X the descr X
    /// - desc_Y the descr y
    /// - body the body elements (track ends)
    ///
    /// __Returns__ an initialized Bezier struct 
    pub fn new(layer: u32, width: u32, color: u32, scale: Scale, visible: bool, 
                X1: f64, Y1: f64, X2: f64, Y2: f64, X3: f64, Y3: f64, 
                X4: f64, Y4: f64, desc_X: f64, desc_Y: f64, body: BezierBody) -> Self {
        Self {layer: layer, width: width, color: color, scale: scale, 
              visible: visible, x1: X1, y1: Y1, x2: X2, y2: Y2, x3: X3, 
              y3: Y3, x4: X4, y4: Y4, desc_x: desc_X, desc_y: desc_Y, 
              body: body }

    }
    pub fn Layer(&self) -> u32 {self.layer}
    pub fn Width(&self) -> u32 {self.width}
    pub fn Color(&self) -> u32 {self.color}
    pub fn Scale(&self) -> Scale {self.scale}
    pub fn Visible(&self) -> bool {self.visible}
    pub fn X1(&self) -> f64 {self.x1}
    pub fn Y1(&self) -> f64 {self.y1}
    pub fn X2(&self) -> f64 {self.x2}
    pub fn Y2(&self) -> f64 {self.y2}
    pub fn X3(&self) -> f64 {self.x3}
    pub fn Y3(&self) -> f64 {self.y3}
    pub fn X4(&self) -> f64 {self.x4}
    pub fn Y4(&self) -> f64 {self.y4}
    pub fn DescX(&self) -> f64 {self.desc_x}
    pub fn DescY(&self) -> f64 {self.desc_y}
    pub fn Body(&self) -> BezierBody {self.body.clone()}
} 


/// Straight track struct 
#[derive(Debug, Clone, PartialEq)]
pub struct Straight {
    layer: u32, 
    line_width: u32, 
    scale: Scale, 
    flags: u32, 
    desc_x: f64, 
    desc_y: f64, 
    body: TrackBody,
}

impl fmt::Display for Straight {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<#Straight {},{}>",self.desc_x,self.desc_y)
    }
}

impl Straight {
    /// Initialize a Straight struct
    /// ## Parameters:
    /// - layer the layer the track is on
    /// - line_width its line width
    /// - scale ist model scale
    /// - flags its flags
    /// - Desc_x its desc X
    /// - Desc_y its descr Y
    /// - body it track ends
    ///
    /// __Returns__ an initialized Straight struct
    pub fn new(layer: u32, line_width: u32, scale: Scale, flags: u32, 
                Desc_x: f64, Desc_y: f64, body: TrackBody) -> Self {
        Self { layer: layer, line_width: line_width, scale: scale, 
                flags: flags, desc_x: Desc_x, desc_y: Desc_y, body: body }
    }
    pub fn Layer(&self) -> u32 {self.layer}
    pub fn LineWidth(&self) -> u32 {self.line_width}
    pub fn Scale(&self) -> Scale {self.scale}
    pub fn Flags(&self) -> u32 {self.flags}
    pub fn DescX(&self) -> f64 {self.desc_x}
    pub fn DescY(&self) -> f64 {self.desc_y}
    pub fn Body(&self) -> TrackBody {self.body.clone()}
}

/// Turnout track struct
#[derive(Debug, Clone, PartialEq)]
pub struct Turnout {
    layer: u32, 
    options: u32, 
    postion: u32, 
    scale: Scale, 
    flags: u32, 
    origx: f64, 
    origy: f64, 
    elev: u32, 
    angle: f64, 
    tablist: String, 
    adjopt: Option<(f64, f64)>, 
    pieropt: Option<(f64, String)>, 
    body: TurnoutBody,
}

impl Turnout {
    /// Initialize a new turnout
    /// ## Parameters:
    /// - layer
    /// - options
    /// - postion
    /// - scale
    /// - flags
    /// - origx
    /// - origy
    /// - elev
    /// - angle
    /// - tablist
    /// - adjopt
    /// - pieropt
    /// - body
    ///
    /// __Returns__ a fresh new Turnout struct
    pub fn new(layer: u32, options: u32, postion: u32, scale: Scale, 
               flags: u32, origx: f64, origy: f64, elev: u32, angle: f64, 
               tablist: String, adjopt: Option<(f64, f64)>, 
               pieropt: Option<(f64, String)>, body: TurnoutBody, ) -> Self {
        Self {layer: layer, options: options, postion: postion, scale: scale, 
              flags: flags, origx: origx, origy: origy, elev: elev, 
              angle: angle, tablist: tablist, adjopt: adjopt,
              pieropt: pieropt, body: body}
    }
} 

/// Turntable track struct
#[derive(Debug, Clone, PartialEq)]
pub struct Turntable {
    layer:u32, 
    scale: Scale, 
    visible: bool, 
    x: f64,
    y: f64, 
    radius: f64, 
    current_ep: Option<u32>, 
    body: TrackBody,
}

impl Turntable {
    /// Initialize a new Turntable
    /// ## Parameters:
    /// - layer
    /// - scale
    /// - visible
    /// - x
    /// - y
    /// - radius
    /// - current_ep
    /// - body
    ///
    /// __Returns__ a fresh Turntable
    pub fn new(layer:u32, scale: Scale, visible: bool, x: f64, y: f64, 
               radius: f64, current_ep: Option<u32>, body: TrackBody) -> Self {
        Self {layer: layer, scale: scale, visible: visible, x: x, y: y, 
              radius: radius, current_ep: current_ep, body: body }
    }
}

/// Joint track struct
#[derive(Debug, Clone, PartialEq)]
pub struct Joint {
    layer: u32, 
    width: u32,
    scale: Scale, 
    flags: u32,
    l0: f64, 
    l1: f64, 
    r: f64, 
    flip: u32, 
    negate: u32,
    s_curve: u32, 
    x: f64, 
    y: f64,
    angle: f64,
    desc_x: f64, 
    desc_y: f64, 
    body: TrackBody,
}

impl Joint {
    /// Initialize a new Joint
    /// ## Parameters:
    /// - layer
    /// - width
    /// - scale
    /// - flags
    /// - l0
    /// - l1
    /// - R
    /// - flip
    /// - negate
    /// - S_curve
    /// - x
    /// - y
    /// - angle
    /// - desc_x
    /// - desc_y
    /// - body
    ///
    /// __Returns__ a newly initialized Joint struct
    pub fn new(layer: u32, width: u32, scale: Scale, flags: u32, l0: f64, 
                l1: f64, R: f64, flip: u32, negate: u32, S_curve: u32, 
                x: f64, y: f64, angle: f64, desc_x: f64, desc_y: f64, 
                body: TrackBody) -> Self {
        Self {layer: layer, width: width, scale: scale, flags: flags, 
              l0: l0, l1: l1, r: R, flip: flip, negate: negate,
              s_curve: S_curve, x: x, y: y, angle: angle, desc_x: desc_x, 
              desc_y: desc_y, body: body}
    }
}

/// Car struct
#[derive(Debug, Clone, PartialEq)]                                              
pub struct Car {
    scale: Scale, 
    title: String, 
    options: u32, 
    typeofcar: u32, 
    length: f64, 
    width: f64, 
    truck_center_offset: u32, 
    truck_center: f64, 
    coupled_length: f64, 
    color: u32, 
    puchaseprice: f64, 
    currentprice: f64, 
    condition: u32, 
    purchdate: u32, 
    servdate: u32, 
    notes: String, 
    onlayout: Option<CarAux>, 
}

impl Car {
    /// Initialize a new car
    /// ## Parameters:
    /// - scale
    /// - title
    /// - options
    /// - typeofcar
    /// - length
    /// - width
    /// - truck_center_offset
    /// - truck_center
    /// - coupled_length
    /// - color
    /// - puchaseprice
    /// - currentprice
    /// - condition
    /// - purchdate
    /// - servdate
    /// - notes
    /// - onlayout
    ///
    /// __Returns__ a feshly initialized Car
    pub fn new(scale: Scale, title: String, options: u32, typeofcar: u32, 
               length: f64, width: f64, truck_center_offset: u32, 
               truck_center: f64, coupled_length: f64, color: u32, 
               puchaseprice: f64, currentprice: f64, condition: u32, 
               purchdate: u32, servdate: u32, notes: String, 
               onlayout: Option<CarAux>) -> Self {
        Self {scale: scale, title: title, options: options, 
              typeofcar: typeofcar, length: length, width: width, 
              truck_center_offset: truck_center_offset, 
              truck_center: truck_center, coupled_length: coupled_length, 
              color: color, puchaseprice: puchaseprice, 
              currentprice: currentprice, condition: condition, 
              purchdate: purchdate, servdate: servdate, notes: notes, 
              onlayout: onlayout}
    }
}


/// Note struct
#[derive(Debug, Clone, PartialEq)]
pub struct Note {
    layer: u32, 
    start_x: f64, 
    start_y: f64, 
    length: u32, 
    typeofnote: u32, 
    text1: String,  
    text2: Option<String>, 
}

impl Note {
    /// Initialize a new Note
    /// ## Parameters:
    /// - layer
    /// - start_x
    /// - start_y
    /// - length
    /// - typeofnote
    /// - text1
    /// - text2
    ///
    /// __Returns__ a new Note
    pub fn new (layer: u32, start_x: f64, start_y: f64, length: u32, 
                typeofnote: u32, text1: String, text2: Option<String>) -> Self {
        Self {layer: layer, start_x: start_x, start_y: start_y, 
              length: length, typeofnote: typeofnote, text1: text1, 
              text2: text2 }
    }
}

/// A text item
#[derive(Debug, Clone, PartialEq)]
pub struct TextItem {
    layer: u32, 
    color: u32, 
    font_size: u32, 
    x: f64, 
    y: f64, 
    check_box: bool,
    text: String, 
    rotation: u32,
}

impl TextItem {
    /// Initialize a Text Item
    /// ## Parameters:
    /// - layer
    /// - color
    /// - font_size
    /// - x
    /// - y
    /// - check_box
    /// - text
    /// - rotation
    ///
    /// __Returns__ an initialized TextItem
    pub fn new(layer: u32, color: u32, font_size: u32, x: f64, y: f64, check_box: bool, text: String, rotation: u32) -> Self {
        Self {layer: layer, color: color, font_size: font_size, x: x, y: y, 
              check_box: check_box, text: text, rotation: rotation }
    }
}

/// A Block
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    name: String, 
    script: String, 
    tracklist: IntegerList,
}

impl Block {
    /// Initialize a new Block
    /// ## Parameters:
    /// - name
    /// - script
    /// - tracklist
    ///
    /// __Returns__ a new Block
    pub fn new(name: String, script: String, tracklist: IntegerList) -> Self {
        Self{name: name, script: script, tracklist: tracklist, }
    }
}

/// A switch motor control
#[derive(Debug, Clone, PartialEq)]
pub struct SwitchMotor {
    turnout: u32,
    name: String,
    normal: String,
    reverse: String,
    pointsense: String,
}

impl SwitchMotor {
    /// Initialize a SwitchMotor
    /// ## Parameters:
    /// - turnout
    /// - name
    /// - normal
    /// - reverse
    /// - pointsense
    ///
    /// __Returns__ a newly initialized SwitchMotor
    pub fn new(turnout: u32, name: String, normal: String, reverse: String, 
                pointsense: String) -> Self {
        Self {turnout: turnout, name: name, normal: normal, reverse: reverse, 
                pointsense: pointsense,}
    }
}

/// A signal
#[derive(Debug, Clone, PartialEq)]
pub struct Signal {
    layer: u32, 
    scale: Scale,
    visible: bool,
    x: f64, 
    y: f64, 
    a: f64, 
    numheads: u32,
    name: String, 
    aspectlist: AspectList,
}

impl Signal {
    /// Initialize a Signal
    /// ## Parameters:
    /// - layer
    /// - scale
    /// - visible
    /// - X
    /// - Y
    /// - A
    /// - numheads
    /// - name
    /// - aspectlist
    ///
    /// __Returns__ An initiaized Signal
    pub fn new(layer: u32, scale: Scale, visible: bool, X: f64, Y: f64, A: f64,
               numheads: u32, name: String, aspectlist: AspectList) -> Self {
        Self {layer: layer, scale: scale, visible: visible, x: X, y: Y, a: A, 
              numheads: numheads, name: name, aspectlist: aspectlist,}
    }
}

/// A sensor
#[derive(Debug, Clone, PartialEq)]
pub struct Sensor {
    layer: u32,
    scale: Scale,
    visible: bool,
    x: f64,
    y: f64,
    name: String,
    script: String,
}

impl Sensor {
    /// Initialize a sensor
    /// ## Parameters:
    /// - layer
    /// - scale
    /// - visible
    /// - X
    /// - Y
    /// - name
    /// script
    ///
    /// __Returns__ an initialized Sensor
    pub fn new(layer: u32, scale: Scale, visible: bool, X: f64, Y: f64, 
                name: String, script: String) -> Self {
        Self {layer: layer, scale: scale, visible: visible, x: X, y: Y, 
                name: name, script: script,}
    }
}

/// A control
#[derive(Debug, Clone, PartialEq)]
pub struct Control {
    layer: u32,
    scale: Scale, 
    visible: bool, 
    start_x: f64, 
    start_y: f64, 
    name: String, 
    on_script: String, 
    off_script: String,
}

impl Control {
    /// Initialize a control
    /// ## Parameters:
    /// - layer
    /// - scale
    /// - visible
    /// - start_x
    /// - start_y
    /// - name
    /// - on_script
    /// - off_script
    ///
    /// __Returns__ an initialized Control
    pub fn new(layer: u32, scale: Scale, visible: bool, start_x: f64, 
                start_y: f64, name: String, on_script: String, 
                off_script: String) -> Self {
        Self {layer: layer, scale: scale, visible: visible, start_x: start_x, 
                start_y: start_y, name: name, on_script: on_script, 
                off_script: off_script}
    }
}


/// Layout structure.  Contains a parsed layout file.
#[derive(Debug)]
pub struct Layout {
    filename: String,
    file_version: u32,
    program_version: (u32,u32,u32),
    title1: String,
    title2: String,
    mapscale: u32,
    roomsize: (f64,f64),
    scale: Scale,
    layers: HashMap<u32,Layer>,
    current_layer: u32,
    structures: HashMap<u32,Structure>,
    drawings: HashMap<u32,Drawing>,
    bzrlines: HashMap<u32,BZRLine>,
    cornus: HashMap<u32,Cornu>,
    curves: HashMap<u32,Curve>,
    beziers: HashMap<u32,Bezier>,
    straights: HashMap<u32,Straight>,
    turnouts: HashMap<u32,Turnout>,
    turntables: HashMap<u32,Turntable>,
    joints: HashMap<u32,Joint>,
    cars: HashMap<u32,Car>,
    notes: HashMap<u32,Note>,
    textitems: HashMap<u32,TextItem>,
    blocks: HashMap<u32,Block>,
    switchmotors: HashMap<u32,SwitchMotor>,
    signals: HashMap<u32,Signal>,
    sensors: HashMap<u32,Sensor>,
    controls: HashMap<u32,Control>,
}

impl fmt::Display for Layout {
    /// Display a Layout
    /// ## Parameters:
    /// - f formatter to write to
    ///
    /// __Returns__ a fmt::Result
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<#Layout {}>", self.title1)
    }
}

impl Layout {
    /// Layout initializer.
    /// Initializes a layout structure by reading and parsing a layout file.
    /// ## Parameters:
    /// - layoutfilename The name of the layout file to process.
    ///
    /// __Returns__ A freshly parsed layout or an error.
    pub fn new(layoutfilename: String) -> Result<Self, LayoutError> {
        let mut this = Self{filename: layoutfilename.clone(), file_version: 0, 
                            program_version: (0,0,0),
                            title1: String::new(), title2: String::new(),
                            mapscale: 1, roomsize: (1.0,1.0),
                            scale: Scale::HO, layers: HashMap::new(),
                            current_layer: 0, structures: HashMap::new(),
                            drawings: HashMap::new(), 
                            bzrlines: HashMap::new(),
                            cornus: HashMap::new(), curves: HashMap::new(),
                            beziers: HashMap::new(), 
                            straights: HashMap::new(),
                            turnouts: HashMap::new(),
                            turntables: HashMap::new(), 
                            joints: HashMap::new(), cars: HashMap::new(),
                            notes: HashMap::new(),
                            textitems: HashMap::new(), blocks: HashMap::new(),
                            switchmotors: HashMap::new(),
                            signals: HashMap::new(), sensors: HashMap::new(), 
                            controls: HashMap::new(),
                            };
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
    /// Set the version information
    /// ## Parameters:
    /// - fver File version
    /// - major major number
    /// - minor minor version number
    /// - release release number
    ///
    /// __Returns__ nothing
    pub fn SetVersion(&mut self,fver: u32, major: u32, minor: u32, 
                      release: u32) {
        self.file_version = fver;
        self.program_version = (major, minor, release);
    }
    /// Set the layout titles
    /// ## Parameters:
    /// - level title level -- should only b 1 or 2
    /// - text the title text
    ///
    /// __Returns__ nothing
    pub fn SetTitle(&mut self,level: u32, text: String) {
        match level {
            1 => {self.title1 = text.trim().to_string();},
            2 => {self.title2 = text.trim().to_string();},
            _ => (),
        };
    }
    /// Set the layout map scale
    /// ## Parameters:
    /// - mapscale the layout map scale
    ///
    /// __Returns__ nothing
    pub fn SetMapscale(&mut self,mapscale: u32) {
        self.mapscale = mapscale;
    }
    /// Set the room size in inches
    /// ## Parameters:
    /// - width room width
    /// - height room height
    ///
    /// __Returns__ nothing
    pub fn SetRoomsize(&mut self,width: f64, height: f64) {
        self.roomsize = (width,height);
    }
    /// Set the layout modeling scale.
    /// ## Parameters:
    /// - scale the scale
    ///
    /// __Returns__ nothing
    pub fn SetScale(&mut self,scale: Scale) {
        self.scale = scale;
    }
    /// Add a layer to the layout
    /// ## Parameters: 
    /// - lnum layer number
    /// - visible is it visible?
    /// - frozen is it frozen?
    /// - on_map is it on map?
    /// - color_rgb its colot
    /// - module its module
    /// - dont_use_color don't use color?
    /// - color_flags color flags
    /// - button_off is it button off?
    /// - name its name
    /// - inherit inherit?
    /// - scale_index its scale index
    /// - min_track_radius minimum track radious
    /// - max_track_grade maximum grade
    /// - tie_length tie length
    /// - tie_width tie width
    /// - tie_spacing tie spacing
    ///
    /// __Returns__ nothing
    pub fn AddLayer(&mut self,lnum: u32,visible: u32,frozen: u32,on_map: u32,
                    color_rgb: u32,module: u32,dont_use_color: u32,
                    color_flags: u32,button_off: u32,name: String,inherit: u32,
                    scale_index: u32,min_track_radius: f64,
                    max_track_grade: f64,tie_length: f64,tie_width: f64,
                    tie_spacing:f64) {
        self.layers.insert(lnum,Layer::new(visible != 0,frozen != 0, 
                                           on_map != 0, color_rgb,module,
                                           dont_use_color != 0, color_flags,
                                           button_off != 0,name,inherit!= 0,
                                           scale_index,min_track_radius,
                                           max_track_grade,tie_length,
                                           tie_width,tie_spacing));
    }
    /// Set the current layer
    /// ## Parameters:
    /// - lnum The current layer
    ///
    /// __Returns__ nothing
    pub fn SetCurrentLayer(&mut self,lnum: u32) {
        self.current_layer = lnum;
    }
    /// Add a structure
    /// - index unique index
    /// - layer the layer the structure is on
    /// - lineType the line type
    /// - pad1 unused
    /// - pad2 unused
    /// - scale the scale
    /// - visible is it visible
    /// - origx orig x
    /// - origy orig y
    /// - elev elevation
    /// - angle the angle
    /// - textfields text fields
    /// - adjopt optional adjustable ends
    /// - pieropt optional piers
    /// - structbody the structure body segments
    ///
    /// __Returns__ nothing
    pub fn AddStructure(&mut self,index: u32, layer: u32, lineType: u32, 
                        pad1: u32, pad2: u32, scale: Scale, visible: u32, 
                        origx: f64, origy: f64, elev: u32, angle: f64, 
                        textfields: String, adjopt: Option<(f64, f64)>, 
                        pieropt: Option<(f64, String)>, 
                        structbody: StructureBody) {
        self.structures.insert(index, Structure::new(layer,lineType,scale,
                                                      visible!=0,origx,origy,
                                                      elev,angle,textfields,
                                                      adjopt,pieropt,
                                                      structbody));
    }
    /// Add a drawing
    /// ## Parameters:
    /// - index The drawing index
    /// - layer The layer the drawing is on
    /// - lineType The line type
    /// - pad1 unused
    /// - pad2 unused
    /// - start_x Starting X
    /// - start_y Starting Y
    /// - start Start?
    /// - angle Angle
    /// - segments body segments
    ///
    /// __Returns__ nothing
    pub fn AddDrawing(&mut self,index: u32, layer: u32, lineType: u32,
                        pad1: u32, pad2: u32, start_x: f64, start_y: f64,
                        start: u32, angle: f64, segments: StructureBody) {
        self.drawings.insert(index,Drawing::new(layer,lineType,start_x,start_y,
                                                start,angle,segments));
    }
    /// Add a BZRLine
    /// ## Parameters:
    /// - index
    /// - layer
    /// - pad1
    /// - pad2
    /// - line_width
    /// - scale
    /// - visible
    /// - X1
    /// - Y1
    /// - X2
    /// - Y2
    /// - X3
    /// - Y3
    /// - X4
    /// - Y4
    /// - pad3
    /// - desc_X
    /// - desc_Y
    /// - body
    /// 
    /// __Returns__ nothing
    pub fn AddBZRLine(&mut self,index: u32, layer: u32, pad1: u32, pad2: u32,
                      line_width: u32, scale: Scale, visible: u32, X1: f64,
                      Y1: f64,X2: f64,Y2: f64,X3: f64,Y3: f64,X4: f64,Y4: f64,
                      pad3: u32,desc_X: f64,desc_Y: f64, body: BZRLineBody) {
        self.bzrlines.insert(index,BZRLine::new(layer,line_width,scale,
                                                visible!=0,X1,Y1,X2,Y2,X3,Y3,
                                                X4,Y4,desc_X,desc_Y,body));
    }
    /// Add a Cornu curve track
    /// ## Parameters:
    /// - index
    /// - layer
    /// - width
    /// - pad1
    /// - pad2
    /// - scale
    /// - visible
    /// - pos1x
    /// - pos1y
    /// - angle1
    /// - radius1
    /// - center1x
    /// - center1y
    /// - pos2x
    /// - pos2y
    /// - angle2
    /// - radius2
    /// - center2x
    /// - center2y
    /// - body: CornuBody 
    ///
    /// __Returns__ nothing
    pub fn AddCornu(&mut self,index: u32,layer: u32,width: u32,pad1: u32,
                    pad2: u32,scale: Scale,visible: u32,pos1x: f64,pos1y: f64,
                    angle1: f64,radius1: f64,center1x: f64,center1y: f64,
                    pos2x: f64,pos2y: f64,angle2: f64,radius2: f64,
                    center2x: f64,center2y: f64,body: CornuBody) {
        self.cornus.insert(index,Cornu::new(layer,width,scale,visible!=0,
                                            pos1x,pos1y,angle1,radius1,
                                            center1x,center1y,pos2x,pos2y,
                                            angle2,radius2,center2x,center2y,
                                            body));
    }
    /// Add a plain curved track
    /// ## Parameters:
    /// - index
    /// - layer
    /// - line_width
    /// - pad1
    /// - pad2
    /// - scale
    /// - flags
    /// - center_X
    /// - centerY
    /// - pad3
    /// - radius
    /// - helix_turns
    /// - desc_X
    /// - desc_Y
    /// - trackbody
    /// 
    /// __Returns__ nothing
    pub fn AddCurve(&mut self,index: u32, layer: u32, line_width: u32, 
                    pad1: u32, pad2: u32, scale: Scale, flags: u32, 
                    center_X: f64, centerY: f64, pad3: u32, radius: f64, 
                    helix_turns: u32, desc_X: f64, desc_Y: f64, 
                    trackbody: TrackBody) {
        self.curves.insert(index, Curve::new(layer, line_width, scale, 
                                             flags, center_X, centerY, radius,
                                             helix_turns, desc_X, desc_Y,
                                             trackbody));
    }
    /// Add a AddBezier curve track
    /// ## Parameters:
    /// - index
    /// - layer
    /// - width
    /// - color
    /// - pad1
    /// - scale
    /// - vis
    /// - X1
    /// - Y1
    /// - X2
    /// - Y2
    /// - X3
    /// - Y3
    /// - X4
    /// - Y4
    /// - pad2
    /// - desc_X
    /// - desc_Y
    /// - body
    /// 
    /// __Returns__ nothing
    pub fn AddBezier(&mut self,index: u32, layer: u32, width: u32, color: u32,
                     pad1: f64, scale: Scale, vis: u32, X1: f64, Y1: f64, 
                     X2: f64, Y2: f64, X3: f64, Y3: f64, X4: f64, Y4: f64, 
                     pad2: u32, desc_X: f64, desc_Y: f64, body: BezierBody) {
        self.beziers.insert(index,Bezier::new(layer, width, color, scale, 
                                              vis!=0, X1, Y1, X2, Y2, X3, Y3,
                                              X4, Y4, desc_X, desc_Y, body));
    }
    /// Add a straight track segment
    /// ##  Parameters:
    /// - index
    /// - layer
    /// - line_width
    /// - pad1
    /// - pad2
    /// - scale
    /// - flags
    /// - Desc_x
    /// - Desc_y
    /// - body
    /// 
    /// __Returns__ nothing
    pub fn AddStraight(&mut self,index: u32, layer: u32, line_width: u32, 
                       pad1: u32, pad2: u32, scale: Scale, flags: u32, 
                       Desc_x: f64, Desc_y: f64, body: TrackBody) {
        self.straights.insert(index,Straight::new(layer, line_width, scale, 
                                                  flags, Desc_x, Desc_y, 
                                                  body));
    }
    /// Add a turnout
    /// ## Parameters:
    /// - index
    /// - layer
    /// - options
    /// - postion
    /// - pad1
    /// - scale
    /// - flags
    /// - origx
    /// - origy
    /// - elev
    /// - angle
    /// - tablist
    /// - adjopt
    /// - pieropt
    /// - body
    ///
    /// __Returns__ nothing
    pub fn AddTurnout(&mut self,index: u32, layer: u32, options: u32, 
                      postion: u32, pad1: u32, scale: Scale, flags: u32, 
                      origx: f64, origy: f64, elev: u32, angle: f64, 
                      tablist: String, adjopt: Option<(f64, f64)>, 
                      pieropt: Option<(f64, String)>, body: TurnoutBody) {
        self.turnouts.insert(index,Turnout::new(layer, options, postion, 
                                                scale, flags, origx, 
                                                origy, elev, angle, tablist, 
                                                adjopt, pieropt, body));
    }
    /// Add a turntable
    /// ## Parameters:
    /// - index
    /// - layer
    /// - pad1
    /// - pad2
    /// - pad3
    /// - scale
    /// - visible
    /// - x
    /// - y
    /// - pad4
    /// - radius
    /// - current_ep, 
    /// - body
    ///
    /// __Returns__ nothing
    pub fn AddTurntable(&mut self,index: u32, layer:u32, pad1: u32, pad2: u32,
                        pad3: u32, scale: Scale, visible: u32, x: f64,
                        y: f64, pad4: u32, radius: f64, 
                        current_ep: Option<u32>, body: TrackBody) {
        self.turntables.insert(index,Turntable::new(layer, scale, visible!=0, 
                                                    x, y, radius, current_ep,
                                                    body));
    }
    /// Add a joint
    /// ## Parameters:
    /// - index
    /// - layer
    /// - width
    /// - pad1
    /// - pad2
    /// - scale
    /// - flags
    /// - l0
    /// - l1
    /// - R
    /// - flip
    /// - negate
    /// - S_curve
    /// - x
    /// - y
    /// - pad3
    /// - angle
    /// - desc_x
    /// - desc_y
    /// - body
    /// 
    /// __Returns__ nothing
    pub fn AddJoint(&mut self,index: u32, layer: u32, width: u32, 
                    pad1: u32, pad2: u32, scale: Scale, flags: u32, 
                    l0: f64, l1: f64, R: f64, flip: u32, negate: u32, 
                    S_curve: u32, x: f64, y: f64, pad3: u32, angle: f64, 
                    desc_x: f64, desc_y: f64, body: TrackBody) {
        self.joints.insert(index,Joint::new(layer,width,scale,flags,l0,l1,R,
                           flip,negate,S_curve,x,y,angle,desc_x,desc_y,body));
    }
    /// Add a car
    /// ## Parameters:
    /// - inx
    /// - scale
    /// - title
    /// - options
    /// - typeofcar
    /// - length
    /// - width
    /// - pad0
    /// - truck_center_offset
    /// - truck_center
    /// - coupled_length
    /// - color
    /// - puchaseprice
    /// - currentprice
    /// - condition
    /// - purchdate
    /// - servdate
    /// - pad1
    /// - pad2
    /// - pad3
    /// - pad4
    /// - pad5
    /// - pad6
    /// - notes
    /// - onlayout
    ///
    /// __Returns__ nothing
    pub fn AddCar(&mut self,inx: u32, scale: Scale, title: String, 
                  options: u32, typeofcar: u32, length: f64, width: f64, 
                  pad0: u32,truck_center_offset: u32, truck_center: f64, 
                  coupled_length: f64, color: u32, puchaseprice: f64,
                  currentprice: f64, condition: u32, purchdate: u32,
                  servdate: u32, pad1: u32,pad2: u32,pad3: u32,pad4: u32,
                  pad5: u32, pad6: u32, notes: String, 
                  onlayout: Option<CarAux>) {
        self.cars.insert(inx,Car::new(scale, title.clone(), options, 
                                      typeofcar, length, width, 
                                      truck_center_offset, truck_center, 
                                      coupled_length, color, puchaseprice, 
                                      currentprice, condition, purchdate, 
                                      servdate, notes.clone(), 
                                      onlayout.clone()));
    }
    /// Add a note
    /// ## Parameters:
    /// - index
    /// - layer
    /// - pad1
    /// - pad2
    /// - start_x
    /// - start_y
    /// - length
    /// - typeofnote
    /// - text1
    /// - text2
    ///
    /// __Returns__ nothing 
    pub fn AddNote(&mut self,index: u32, layer: u32, pad1: u32, pad2: u32,
                   start_x: f64, start_y: f64, length: u32, typeofnote: u32,
                   text1: String,  text2: Option<String>) {
        self.notes.insert(index,Note::new(layer, start_x, start_y, length, 
                                          typeofnote, text1.clone(),  
                                          text2.clone()));
    }
    /// Add a Text item
    /// ## Parameters:
    /// - index
    /// - layer
    /// - color
    /// - font_size
    /// - pad1
    /// - x
    /// - y
    /// - check_box
    /// - text
    /// - rotation
    ///
    /// __Returns__ nothing 
    pub fn AddText(&mut self,index: u32, layer: u32, color: u32, 
                   font_size: u32, pad1: u32, x: f64, y: f64, check_box: u32,
                   text: String, rotation: u32) {
        self.textitems.insert(index,TextItem::new(layer, color, font_size,
                                                  x, y, check_box!=0, 
                                                  text.clone(), rotation));
    }
    /// Add a block
    /// ## Parameters:
    /// - index
    /// - name
    /// - script
    /// - tracklist
    ///
    /// __Returns__ nothing 
    pub fn AddBlock(&mut self,index: u32, name: String, script: String, tracklist: IntegerList) {
        self.blocks.insert(index,Block::new(name.clone(), script.clone(), 
                                            tracklist.clone()));
    }
    /// Add a Switch motor
    /// - index
    /// - turnout
    /// - name
    /// - normal
    /// - reverse
    /// - pointsense
    ///
    /// __Returns__ nothing
    pub fn AddSwitchMotor(&mut self,index: u32,turnout: u32,name: String,
                          normal: String ,reverse: String,pointsense: String) {
        self.switchmotors.insert(index,SwitchMotor::new(turnout,name.clone(),
                                                        normal.clone(),
                                                        reverse.clone(),
                                                        pointsense.clone()));
    }
    /// Add a signal
    /// ## Parameters:
    /// - index
    /// - layer
    /// - scale
    /// - visible
    /// - X
    /// - Y
    /// - A
    /// - numheads
    /// - name
    /// - aspectlist
    ///
    /// __Returns__ nothing
    pub fn AddSignal(&mut self,index: u32,layer: u32, scale: Scale, 
                     visible: u32, X: f64, Y: f64, A: f64, numheads: u32,
                     name: String, aspectlist: AspectList) {
        self.signals.insert(index,Signal::new(layer,scale,visible!=0,X,Y,A,
                                              numheads,name.clone(),
                                              aspectlist.clone()));
    }
    /// Add a sensor
    /// ## Parameters:
    /// - index
    /// - layer
    /// - scale
    /// - visible
    /// - X
    /// - Y
    /// - name
    /// - script
    ///
    /// __Returns__ nothing
    pub fn AddSensor(&mut self,index: u32,layer: u32,scale: Scale,visible: u32,
                     X: f64,Y: f64,name: String,script: String) {
        self.sensors.insert(index,Sensor::new(layer,scale,visible!=0,X,Y,
                                              name.clone(),script.clone()));
    }
    /// Add a control
    /// ## Parameters:
    /// - index
    /// - layer
    /// - scale
    /// - visible
    /// - start_x
    /// - start_y
    /// - name
    /// - on_script
    /// - off_script
    ///
    /// __Returns__ nothing
    pub fn AddControl(&mut self,index: u32,layer: u32,scale: Scale,
                     visible: u32, start_x: f64, start_y: f64, 
                     name: String, on_script: String, off_script: String) {
        self.controls.insert(index,Control::new(layer,scale,visible!=0,start_x,
                                                start_y,name.clone(),
                                                on_script.clone(),
                                                off_script.clone()));
    }
    /// Return the layout filename
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout filename
    pub fn Filename(&self) -> String {self.filename.clone()}
    /// Return the layout file version
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout file version
    pub fn FileVersion(&self) -> u32 {self.file_version}
    /// Return the layout program version
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout program version
    pub fn ProgramVersion(&self) -> (u32,u32,u32) {self.program_version}
    /// Return the layout main title
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout main title
    pub fn Title(&self) -> String {self.title1.clone()}
    /// Return the layout subtitle
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout subtitle
    pub fn SubTitle(&self) -> String {self.title2.clone()}
    /// Return the layout map scale
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout filename
    pub fn MapScale(&self) -> u32 {self.mapscale}
    /// Return the layout room size
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout room size
    pub fn RoomSize(&self) -> (f64,f64) {self.roomsize}
    /// Return the layout scale
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout scale
    pub fn Scale(&self) -> Scale {self.scale}
    /// Return the layout's layer indexes
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layer indexes
    pub fn LayerIndexes(&self) -> Vec<&u32> {
        self.layers.keys().collect()
    }
    /// Return a layout layer
    /// ## Parameters:
    /// - i the layer number
    ///
    /// __Returns__ the layout's ith layer 
    pub fn Layer(&self, i: u32) -> Option<&Layer> {self.layers.get(&i)}
    /// Return a layer iter
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ a Iterator into the layers
    pub fn LayerIter(&self) -> impl Iterator<Item = (&u32, &Layer)> {
        self.layers.iter()
    } 
    /// Return the current layer
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the current layer
    pub fn CurrentLayerNumber(&self) -> u32 {self.current_layer}
    /// Return the layout's structure indexes
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout's structure indexes 
    pub fn StructureIndexes(&self) -> Vec<&u32> {
        self.structures.keys().collect()
    }
    /// Return a layout Structure
    /// ## Parameters:
    /// - i the Structure number
    ///
    /// __Returns__ the layout's ith Structure 
    pub fn Structure(&self, i: u32) -> Option<&Structure> {self.structures.get(&i)}
    /// Return a structures iter
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ a Iterator into the structures
    pub fn StructureIter(&self) -> impl Iterator<Item = (&u32, &Structure)> {
        self.structures.iter()
    } 
    /// Return the layout's drawing indexes
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout's drawing indexes 
    pub fn DrawingIndexes(&self) -> Vec<&u32> {
        self.drawings.keys().collect()
    }
    /// Return a layout Drawing
    /// ## Parameters:
    /// - i the Drawing number
    ///
    /// __Returns__ the layout's ith Drawing 
    pub fn Drawing(&self, i: u32) -> Option<&Drawing> {self.drawings.get(&i)}
    /// Return a drawings iter
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ a Iterator into the drawings
    pub fn DrawingIter(&self) -> impl Iterator<Item = (&u32, &Drawing)> {
        self.drawings.iter()
    } 
    /// Return the layout's bzrline indexes
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout's bzrline indexes 
    pub fn BZRLineIndexes(&self) -> Vec<&u32> {
        self.bzrlines.keys().collect()
    }
    /// Return a layout BZRLine
    /// ## Parameters:
    /// - i the BZRLine number
    ///
    /// __Returns__ the layout's ith BZRLine 
    pub fn BZRLine(&self, i: u32) -> Option<&BZRLine> {self.bzrlines.get(&i)}
    /// Return a bzrlines iter
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ a Iterator into the structures
    pub fn BZRLineIter(&self) -> impl Iterator<Item = (&u32, &BZRLine)> {
        self.bzrlines.iter()
    } 
    /// Return the layout's cornu indexes
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout's cornu indexes 
    pub fn CornuIndexes(&self) -> Vec<&u32> {
        self.cornus.keys().collect()
    }
    /// Return a layout Cornu
    /// ## Parameters:
    /// - i the Cornu number
    ///
    /// __Returns__ the layout's ith Cornu 
    pub fn Cornu(&self, i: u32) -> Option<&Cornu> {self.cornus.get(&i)}
    /// Return a cornus iter
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ a Iterator into the cornus
    pub fn CornuIter(&self) -> impl Iterator<Item = (&u32, &Cornu)> {
        self.cornus.iter()
    } 
    /// Return the layout's curve indexes
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout's curve indexes 
    pub fn CurveIndexes(&self) -> Vec<&u32> {
        self.curves.keys().collect()
    }
    /// Return a layout Curve
    /// ## Parameters:
    /// - i the Curve number
    ///
    /// __Returns__ the layout's ith Curve 
    pub fn Curve(&self, i: u32) -> Option<&Curve> {self.curves.get(&i)}
    /// Return a curves iter
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ a Iterator into the curves
    pub fn CurveIter(&self) -> impl Iterator<Item = (&u32, &Curve)> {
        self.curves.iter()
    } 
    /// Return the layout's bezier indexes
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout's bezier indexes 
    pub fn BezierIndexes(&self) -> Vec<&u32> {
        self.beziers.keys().collect()
    }
    /// Return a layout Bezier
    /// ## Parameters:
    /// - i the Bezier number
    ///
    /// __Returns__ the layout's ith Bezier 
    pub fn Bezier(&self, i: u32) -> Option<&Bezier> {self.beziers.get(&i)}
    /// Return a beziers iter
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ a Iterator into the beziers
    pub fn BezierIter(&self) -> impl Iterator<Item = (&u32, &Bezier)> {
        self.beziers.iter()
    } 
    /// Return the layout's straight indexes
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout's straight indexes 
    pub fn StraightIndexes(&self) -> Vec<&u32> {
        self.straights.keys().collect()
    }
    /// Return a layout Straight
    /// ## Parameters:
    /// - i the Straight number
    ///
    /// __Returns__ the layout's ith Straight 
    pub fn Straight(&self, i: u32) -> Option<&Straight> {self.straights.get(&i)}
    /// Return a straights iter
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ a Iterator into the straights
    pub fn StraightIter(&self) -> impl Iterator<Item = (&u32, &Straight)> {
        self.straights.iter()
    } 
    /// Return the layout's turnout indexes
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout's turnout indexes 
    pub fn TurnoutIndexes(&self) -> Vec<&u32> {
        self.turnouts.keys().collect()
    }
    /// Return a layout Turnout
    /// ## Parameters:
    /// - i the Turnout number
    ///
    /// __Returns__ the layout's ith Turnout 
    pub fn Turnout(&self, i: u32) -> Option<&Turnout> {self.turnouts.get(&i)}
    /// Return a turnouts iter
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ a Iterator into the turnouts
    pub fn TurnoutIter(&self) -> impl Iterator<Item = (&u32, &Turnout)> {
        self.turnouts.iter()
    } 
    /// Return the layout's turntable indexes
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout's turntable indexes 
    pub fn TurntableIndexes(&self) -> Vec<&u32> {
        self.turntables.keys().collect()
    }
    /// Return a layout Turntable
    /// ## Parameters:
    /// - i the Turntable number
    ///
    /// __Returns__ the layout's ith Turntable 
    pub fn Turntable(&self, i: u32) -> Option<&Turntable> {self.turntables.get(&i)}
    /// Return a turntables iter
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ a Iterator into the turntables
    pub fn TurntableIter(&self) -> impl Iterator<Item = (&u32, &Turntable)> {
        self.turntables.iter()
    } 
    /// Return the layout's joint indexes
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout's joint indexes 
    pub fn JointIndexes(&self) -> Vec<&u32> {
        self.joints.keys().collect()
    }
    /// Return a layout Joint
    /// ## Parameters:
    /// - i the Joint number
    ///
    /// __Returns__ the layout's ith Joint 
    pub fn Joint(&self, i: u32) -> Option<&Joint> {self.joints.get(&i)}
    /// Return a joints iter
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ a Iterator into the joints
    pub fn JointIter(&self) -> impl Iterator<Item = (&u32, &Joint)> {
        self.joints.iter()
    } 
    /// Return the layout's car indexes
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout's car indexes 
    pub fn CarIndexes(&self) -> Vec<&u32> {
        self.cars.keys().collect()
    }
    /// Return a layout Car
    /// ## Parameters:
    /// - i the Car number
    ///
    /// __Returns__ the layout's ith Car 
    pub fn Car(&self, i: u32) -> Option<&Car> {self.cars.get(&i)}
    /// Return a cars iter
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ a Iterator into the cars
    pub fn CarIter(&self) -> impl Iterator<Item = (&u32, &Car)> {
        self.cars.iter()
    } 
    /// Return the layout's note indexes
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout's note indexes 
    pub fn NoteIndexes(&self) -> Vec<&u32> {
        self.notes.keys().collect()
    }
    /// Return a layout Note
    /// ## Parameters:
    /// - i the Note number
    ///
    /// __Returns__ the layout's ith Note 
    pub fn Note(&self, i: u32) -> Option<&Note> {self.notes.get(&i)}
    /// Return a notes iter
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ a Iterator into the notes
    pub fn NoteIter(&self) -> impl Iterator<Item = (&u32, &Note)> {
        self.notes.iter()
    } 
    /// Return the layout's textitem indexes
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout's textitem indexes 
    pub fn TextItemIndexes(&self) -> Vec<&u32> {
        self.textitems.keys().collect()
    }
    /// Return a layout TextItem
    /// ## Parameters:
    /// - i the TextItem number
    ///
    /// __Returns__ the layout's ith TextItem 
    pub fn TextItem(&self, i: u32) -> Option<&TextItem> {self.textitems.get(&i)}
    /// Return a textitems iter
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ a Iterator into the textitems
    pub fn TextItemIter(&self) -> impl Iterator<Item = (&u32, &TextItem)> {
        self.textitems.iter()
    } 
    /// Return the layout's block indexes
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout's block indexes 
    pub fn BlockIndexes(&self) -> Vec<&u32> {
        self.blocks.keys().collect()
    }
    /// Return a layout Block
    /// ## Parameters:
    /// - i the Block number
    ///
    /// __Returns__ the layout's ith Block 
    pub fn Block(&self, i: u32) -> Option<&Block> {self.blocks.get(&i)}
    /// Return a blocks iter
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ a Iterator into the blocks
    pub fn BlockIter(&self) -> impl Iterator<Item = (&u32, &Block)> {
        self.blocks.iter()
    } 
    /// Return the layout's switchmotor indexes
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout's switchmotor indexes 
    pub fn SwitchMotorIndexes(&self) -> Vec<&u32> {
        self.switchmotors.keys().collect()
    }
    /// Return a layout SwitchMotor
    /// ## Parameters:
    /// - i the SwitchMotor number
    ///
    /// __Returns__ the layout's ith SwitchMotor 
    pub fn SwitchMotor(&self, i: u32) -> Option<&SwitchMotor> {self.switchmotors.get(&i)}
    /// Return a switchmotors iter
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ a Iterator into the switchmotors
    pub fn SwitchMotorIter(&self) -> impl Iterator<Item = (&u32, &SwitchMotor)> {
        self.switchmotors.iter()
    } 
    /// Return the layout's signal indexes
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout's signal indexes 
    pub fn SignalIndexes(&self) -> Vec<&u32> {
        self.signals.keys().collect()
    }
    /// Return a layout Signal
    /// ## Parameters:
    /// - i the Signal number
    ///
    /// __Returns__ the layout's ith Signal 
    pub fn Signal(&self, i: u32) -> Option<&Signal> {self.signals.get(&i)}
    /// Return a signals iter
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ a Iterator into the signals
    pub fn SignalIter(&self) -> impl Iterator<Item = (&u32, &Signal)> {
        self.signals.iter()
    } 
    /// Return the layout's sensor indexes
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout's sensor indexes 
    pub fn SensorIndexes(&self) -> Vec<&u32> {
        self.sensors.keys().collect()
    }
    /// Return a layout Sensor
    /// ## Parameters:
    /// - i the Sensor number
    ///
    /// __Returns__ the layout's ith Sensor 
    pub fn Sensor(&self, i: u32) -> Option<&Sensor> {self.sensors.get(&i)}
    /// Return a sensors iter
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ a Iterator into the sensors
    pub fn SensorIter(&self) -> impl Iterator<Item = (&u32, &Sensor)> {
        self.sensors.iter()
    } 
    /// Return the layout's control indexes
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the layout's control indexes 
    pub fn ControlIndexes(&self) -> Vec<&u32> {
        self.controls.keys().collect()
    }
    /// Return a layout Control
    /// ## Parameters:
    /// - i the Control number
    ///
    /// __Returns__ the layout's ith Control 
    pub fn Control(&self, i: u32) -> Option<&Control> {self.controls.get(&i)}
    /// Return a controls iter
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ a Iterator into the controls
    pub fn ControlIter(&self) -> impl Iterator<Item = (&u32, &Control)> {
        self.controls.iter()
    } 
}

/// Standard scales
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Scale {
    HO,
    N,
    O,
    G,
}

impl fmt::Display for Scale {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Scale::HO => write!(f,"HO"),
            Scale::N  => write!(f,"N"),
            Scale::O  => write!(f,"O"),
            Scale::G  => write!(f,"G"),
        }
    }
}


/// BZSegment elements
#[derive(Debug, PartialEq, Clone)]
pub enum BZSegment {
    S1(u32,f64,f64,f64,f64,f64),
    S2(u32,u32,f64,f64,f64,f64,f64,f64,f64),
    C1(u32,f64,f64,f64,f64,f64,f64),
    C2(u32,u32,f64,f64,f64,f64,f64,f64,f64),
}

/// BZSegments list struct
#[derive(Debug, PartialEq, Clone)] 
pub struct BZSegments {
    elements: Vec<BZSegment>,
}

impl BZSegments {
    /// Create a new empty list of BZSegments
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ an empty BZSegments struct
    pub fn new() -> Self {
        Self {elements: Vec::new()}
    }
    /// Append a BZSegment to a list of BZSegments
    /// ## Parameters:
    /// - e a new BZSegment
    /// - b a BZSegments list struct
    ///
    /// __Returns__ the updated list.
    pub fn AppendBZSeg(e: BZSegment, mut b: BZSegments) -> Self {
        b.elements.insert(0,e);
        b
    }
}


/// BZLSegments list.
#[derive(Debug, PartialEq, Clone)]
pub struct BZLSegments {
    elements: Vec<StructureBodyElement>,
}

impl BZLSegments {
    /// Initialize a new empty BZLSegments list.
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ a new empty BZLSegments struct
    pub fn new() -> Self {
        Self{elements: Vec::new()}
    }
    /// Append a StructureBodyElement to a BZLSegments list
    /// ## Parameters:
    /// - e a StructureBodyElement to add to the list
    /// - b the BZLSegments list to update
    ///
    /// __Returns__ the updated list.
    pub fn Append(e: StructureBodyElement, mut b:BZLSegments) -> Self {
        b.elements.insert(0,e);
        b
    }
    /// Number of segments
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the number of segments
    pub fn len(&self) -> usize {self.elements.len()}
}

impl fmt::Display for BZLSegments { 
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<#BZLSegments {} elements>", self.elements.len())
    }
}

/// A FBlock element
#[derive(Debug, PartialEq, Clone)] 
pub struct FBlockElement(f64,f64,u32);

impl fmt::Display for FBlockElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<#FBlockElement {} {} {}>",self.0,self.1,self.2)
    }
}

/// A FBlock liist
#[derive(Debug, PartialEq, Clone)]
pub struct FBlock {
    elements: Vec<FBlockElement>
}

impl fmt::Display for FBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<#FBlock {} elements>", self.elements.len())
    }
}

impl FBlock {
    /// Initialize an empty FBlock
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ an initialized and empty FBlock
    pub fn new() -> Self {
        //eprintln!("*** FBlock::new()");
        Self{ elements: Vec::new()}
    }
    /// Add a new FBlock element
    /// ## Parameters:
    /// - e a FBlock element to add
    /// - b the list to update
    ///
    /// __Returns__ the updated FBlock list
    pub fn Append(e: FBlockElement, mut b: FBlock) -> Self {
        //eprintln!("*** FBlock::Append({:?},{:?})",e,b);
        b.elements.insert(0,e);
        b
    }
    /// The number of body elements
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the number of elements
    pub fn len(&self) -> usize {self.elements.len()}
}

impl Index<usize> for FBlock {
    type Output = FBlockElement;
    fn index(&self, i: usize) -> &FBlockElement {&self.elements[i]}
}

/// A Structure Body Element
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

impl fmt::Display for StructureBodyElement {
    fn fmt(&self, fp: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StructureBodyElement::D(a,b) =>
                write!(fp, "<#StructureBodyElement::D({},{})>",a,b),
            StructureBodyElement::L(a,b,c,d,e,f,g,h,i) =>
                write!(fp, "<#StructureBodyElement::L({},{},{},{},{},{},{},{},{})>",
                                                    a,b,c,d,e,f,g,h,i),
            StructureBodyElement::M(a,b,c,d,e,f,g,h,i,j) => 
                write!(fp, "<#StructureBodyElement::M({},{},{},{},{},{},{},{},{},{})>",
                                                    a,b,c,d,e,f,g,h,i,j),
            StructureBodyElement::F(a,b,c,d,e,f) =>
                write!(fp, "<#StructureBodyElement::F({},{},{},{},{},{})>",
                                                    a,b,c,d,
                        match e {
                            None => String::from("None"),
                            Some(v) => format!("Some({})", v),
                        },f),
            StructureBodyElement::A(a,b,c,d,e,f,g,h,i) =>
                write!(fp, "<#StructureBodyElement::A({},{},{},{},{},{},{},{},{})>",
                                                    a,b,c,d,e,f,g,h,i),
            StructureBodyElement::B(a,b,c,d,e,f,g,h,i,j) =>
                write!(fp, "<#StructureBodyElement::B({},{},{},{},{},{},{},{},{},{})>",
                                                    a,b,c,d,e,f,g,h,i,j),
            StructureBodyElement::Q(a,b,c,d,e,f,g,h,i) =>
                write!(fp, "<#StructureBodyElement::Q({},{},{},{},{},{},{},{},{})>",
                                                    a,b,c,d,e,f,g,h,i),
            StructureBodyElement::G(a,b,c,d,e,f,g) =>
                write!(fp, "<#StructureBodyElement::G({},{},{},{},{},{},{})>",
                                                    a,b,c,d,e,f,g),
            StructureBodyElement::Y(a,b,c,d,e,f) =>
                write!(fp, "<#StructureBodyElement::Y({},{},{},{},{},{})>",
                                                    a,b,c,d,e,f),
            StructureBodyElement::Z(a,b,c,d,e,f,g) =>
                write!(fp, "<#StructureBodyElement::Z({},{},{},{},{},{},{})>",
                                                    a,b,c,d,e,f,g),
            StructureBodyElement::H(a,b,c,d,e,f,g,h,i,j,k) =>
                write!(fp, "<#StructureBodyElement::H({},{},{},{},{},{},{},{},{},{},{})>",
                                                a,b,c,d,e,f,g,h,i,j,k),
        }
    }
}

/// A list of Structure Body Elements
#[derive(Debug, Clone, PartialEq, Default)]
pub struct StructureBody {
    elements: Vec<StructureBodyElement>,
}

impl fmt::Display for StructureBody {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<#StructureBody {} elements>", self.elements.len())
    }
}

impl StructureBody {
    /// Initialize a StructureBody
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ an empty StructureBody
    pub fn new() -> Self {
        //eprintln!("*** StructureBody::new()");
        Self {elements: Vec::new()}
    }
    /// Add an element to a StructureBody
    /// ## Parameters:
    /// - e the new element
    /// - b the body to update
    ///
    /// __Returns__ the updated body
    pub fn Append(e: StructureBodyElement, mut b: StructureBody) -> Self {
        //eprintln!("*** StructureBody::Append({:?},{:?})",e,b);
        b.elements.insert(0,e);
        b
    }
    /// The number of body elements
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the number of elements
    pub fn len(&self) -> usize {self.elements.len()}
}

impl Index<usize> for StructureBody {
    type Output = StructureBodyElement;
    fn index(&self, i: usize) -> &StructureBodyElement {&self.elements[i]}
}

/// A BZRLineBody Struct
#[derive(Debug, Clone, PartialEq)]
pub struct BZRLineBody {
    elements: Vec<StructureBodyElement>,
}

impl fmt::Display for BZRLineBody {
    /// Display a BZRLineBody
    /// ## Parameters:
    /// - f formatter to write to
    ///
    /// __Returns__ a fmt::Result
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<#BZRLineBody {} elements>", self.elements.len())
    }
}

impl BZRLineBody {
    /// Initialize a BZRLineBody Struct
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ a fresh empty BZRLineBody Struct
    pub fn new() -> Self {
        Self{ elements: Vec::new()}
    }
    /// Add a StructureBodyElement to a BZRLineBody Struct
    /// ## Parameters:
    /// - e a StructureBodyElement to add
    /// - b the body to update
    ///
    /// __Returns__ the updated body
    pub fn Append(e: StructureBodyElement, mut b: BZRLineBody) -> Self {
        b.elements.insert(0,e);
        b
    }
    /// Number of elements
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ the number of elemnts
    pub fn len(&self) -> usize {self.elements.len()}
}

impl Index<usize> for BZRLineBody {
    type Output = StructureBodyElement;
    fn index(&self, i: usize) -> &StructureBodyElement {&self.elements[i]}
}

/// A CornuBodyElement
#[derive(Debug, Clone, PartialEq)]
pub struct CornuBodyElement(u32,u32,f64,f64,f64,f64,f64,f64,f64,f64,f64,
                            BZSegments);

/// A Cornu body
#[derive(Debug, Clone, PartialEq)]
pub struct CornuBody {
    trackends: Vec<TrackBodyElement>,
    trackelements: Vec<CornuBodyElement>,
}

impl CornuBody {
    /// Initialize a Cornu body struct
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ a fresh empty Cornu body
    pub fn new() -> Self {
        Self{ trackends: Vec::new(), trackelements: Vec::new(),}
    }
    /// Add a track body element (track ends) to a Cornu body struct
    /// ## Parameters:
    /// - e a TrackBodyElement (track end)
    /// - b the Cornu body struct to update
    ///
    /// __Returns__ the updated Cornu body struct
    pub fn AppendTrack(e: TrackBodyElement, mut b: CornuBody) -> Self {
        b.trackends.insert(0,e);
        b
    }
    /// Add a CornuBodyElement to a Cornu body struct 
    /// ## Parameters:
    /// - e a Cornu body element
    /// - b a Cornu body struct  to update
    ///
    /// __Returns__ the updated body.
    pub fn AppendCornu(e: CornuBodyElement, mut b: CornuBody) -> Self {
        b.trackelements.insert(0,e);
        b
    }
}

/// A Float (f64) or String
#[derive(Debug, Clone, PartialEq)]
pub enum FloatOrString {
    Float(f64),
    String(String),
}

/// A track body element (track ends)
#[derive(Debug, Clone, PartialEq)]
pub enum TrackBodyElement {
    T1(u32,f64,f64,f64,Option<TrackBodySubElement>),
    T4(u32,u32,f64,f64,f64,TrackBodySubElement),
    E1(f64,f64,f64,Option<TrackBodySubElement>),
    E4(u32,f64,f64,f64,TrackBodySubElement),
}

/// A track body sub element
#[derive(Debug, Clone, PartialEq)]
pub enum TrackBodySubElement {
    T1(u32,f64,f64,Option<FloatOrString>),
    T4(u32,f64,f64,FloatOrString,f64,u32,u32,u32,f64),
}

/// A track body (conventual track)
#[derive(Debug, Clone, PartialEq)]
pub struct TrackBody {
    elements: Vec<TrackBodyElement>,
}

impl TrackBody {
    /// Initialize a track body
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ an empty track body
    pub fn new() -> Self {
        Self{ elements: Vec::new(),}
    }
    /// Add a track end to a track body
    /// ## Parameters:
    /// - e a track body element (a track end)
    /// - b the track body to update
    ///
    /// __Returns__ the updated body
    pub fn AppendTrack(e:TrackBodyElement, mut b: TrackBody) -> Self {
        b.elements.insert(0,e);
        b
    }
}

/// A Bezier Body
#[derive(Debug, Clone, PartialEq)]
pub struct BezierBody {
    elements: Vec<BezierBodyElement>,
}

impl BezierBody {
    /// Initialize a Bezier Body
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ an empty Bezier Body
    pub fn new() -> Self {
        Self {elements: Vec::new()}
    }
    /// Add an element to Bezier Body
    /// ## Parameters:
    /// - e an element
    /// - b the body to update
    ///
    /// __Returns__ the updated body
    pub fn Append(e: BezierBodyElement, mut b: BezierBody) -> Self {
        b.elements.insert(0,e);
        b
    }
}

/// Bezier Body elements
#[derive(Debug, Clone, PartialEq)]
pub enum BezierBodyElement {
    Curve1(u32,f64,f64,f64,f64,f64,f64),
    Curve2(u32,u32,f64,f64,f64,f64,f64,f64,f64),
    Straight1(u32,f64,f64,f64,f64,f64),
    Straight2(u32,u32,f64,f64,f64,f64,f64,f64,f64),
    T1(u32,f64,f64,f64,Option<TrackBodySubElement>),
    T4(u32,u32,f64,f64,f64,TrackBodySubElement),
    E1(f64,f64,f64,Option<TrackBodySubElement>),
    E4(u32,f64,f64,f64,TrackBodySubElement),
}

impl BezierBodyElement {
    /// Make a track end into a Bezier Body element
    /// ## Parameters:
    /// - e the track end
    ///
    /// __Returns__ a Bezier Body element.
    pub fn MakeTrackEnd(e:TrackBodyElement) -> BezierBodyElement {
        match e {
        TrackBodyElement::T1(a,b,c,d,e) =>
            BezierBodyElement::T1(a,b,c,d,e),
        TrackBodyElement::T4(a2,b,c,d,e,f) =>
            BezierBodyElement::T4(a2,b,c,d,e,f),
        TrackBodyElement::E1(a,b,c,f) =>
            BezierBodyElement::E1(a,b,c,f),
        TrackBodyElement::E4(a,b,c,f,g) =>
            BezierBodyElement::E4(a,b,c,f,g),
        }
    }    
}

/// A list of integers (u32)
#[derive(Debug, Clone, PartialEq)]
pub struct IntegerList {
    elements: Vec<u32>,
}

impl IntegerList {
    /// Initialize an IntegerList
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ an empty IntegerList
    pub fn new() -> Self {
        Self { elements: Vec::new() }
    }
    /// Add an integer to the list
    /// ## Parameters:
    /// - e the integer to add
    /// - b the list to update
    ///
    /// __Returns__ the updated list
    pub fn Append(e: u32, mut b: IntegerList) -> Self {
        b.elements.insert(0,e);
        b
    }
}

/// A turnout body
#[derive(Debug, Clone, PartialEq)]
pub struct TurnoutBody {
    turnout_elements: Vec<TurnoutBodyElement>,
    struct_elements:  Vec<StructureBodyElement>,
}

impl TurnoutBody {
    /// Initialize a TurnoutBody
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ an empty TurnoutBody
    pub fn new() -> Self {
        Self { turnout_elements: Vec::new(), struct_elements: Vec::new(),}
    }
    /// Add a track turnout element to the TurnoutBody
    /// ## Parameters:
    /// - e the turnout element to add
    /// - b the TurnoutBody
    ///
    /// __Returns__ the updated turnout body
    pub fn AppendTurnoutBodyElement(e: TurnoutBodyElement,
                                    mut b: TurnoutBody) -> Self {
        b.turnout_elements.insert(0,e);
        b
    }
    /// Add a structure body element to the TurnoutBody (Structures that
    /// include tracks, eg bridges, etc.) are represented a turnouts with a
    /// single path.)
    /// ## Parameters:
    /// - e the StructureBodyElement to add
    /// - b the TurnoutBody
    ///
    /// __Returns__ the updated turnout body
    pub fn AppendStructureBodyElement(e: StructureBodyElement,
                                      mut b: TurnoutBody) -> Self {
        b.struct_elements.insert(0,e); 
        b
    }
}

/// TurnoutBodyElements
#[derive(Debug, Clone, PartialEq)]
pub enum TurnoutBodyElement {
    D(f64,f64),
    P(String,IntegerList),
    S1(u32,f64,f64,f64,f64,f64),
    S2(u32,u32,f64,f64,f64,f64,f64,f64,f64),
    C1(u32,f64,f64,f64,f64,f64,f64),
    C2(u32,u32,f64,f64,f64,f64,f64,f64,f64),
    J1(u32,f64,f64,f64,f64,f64,f64,f64,f64,u32),
    J2(u32,u32,f64,f64,f64,f64,f64,f64,f64,f64,f64,u32),
    T1(u32,f64,f64,f64,Option<TrackBodySubElement>),
    T4(u32,u32,f64,f64,f64,TrackBodySubElement),
    E1(f64,f64,f64,Option<TrackBodySubElement>),
    E4(u32,f64,f64,f64,TrackBodySubElement),
}

impl TurnoutBodyElement {
    /// Convert track ends
    /// ## Parameters:
    /// - tbelt The track end
    ///
    /// __Returns__ the converted track end
    pub fn MakeTurnoutEnd(tbelt: TrackBodyElement) -> Self {
        match tbelt {
        TrackBodyElement::T1(a,b,c,d,e) =>
            TurnoutBodyElement::T1(a,b,c,d,e),
        TrackBodyElement::T4(a2,b,c,d,e,f) =>
            TurnoutBodyElement::T4(a2,b,c,d,e,f),
        TrackBodyElement::E1(a,b,c,f) =>
            TurnoutBodyElement::E1(a,b,c,f),
        TrackBodyElement::E4(a,b,c,f,g) =>
            TurnoutBodyElement::E4(a,b,c,f,g),
        }
    }
}

/// CarAux -- car on layout information
#[derive(Debug, Clone, PartialEq)]
pub struct CarAux(u32,u32,f64,f64,f64,TrackBody);

/// Signal aspect
#[derive(Debug, Clone, PartialEq)]
pub struct Aspect(String,String);

/// Signal aspect list
#[derive(Debug, Clone, PartialEq)]
pub struct AspectList {
    aspects: Vec<Aspect>,
}

impl AspectList {
    /// Initialize an AspectList
    /// ## Parameters:
    /// None
    ///
    /// __Returns__ an empty AspectList
    pub fn new() -> Self {
        Self{aspects: Vec::new()}
    }
    /// Add an aspect to an AspectList
    /// ## Parameters:
    /// - b the AspectList
    /// - a the aspect name
    /// - s the aspect script
    pub fn AddAspect(mut b: AspectList, a: String, s: String) -> Self {
        b.aspects.insert(0,Aspect(a,s));
        b
    }
}
