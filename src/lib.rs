use lalrpop_util::lalrpop_mod;
use std::fs::File;
use std::io::prelude::*;

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

use std::str::CharIndices;

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Clone)]
pub enum Tok {
    RESTOFLINE,
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


pub enum LexicalError {
    UnTerminatedString,
    UnknownKeyword,
    IOError(String),
}

pub struct Lexer<'input> {
    reader: BufReader<'input>,
    scaneol: bool,
    lineno: u32,
    chars: CharIndices<'input>,
    first_column: u32,
    current_column: u32,
    needline: bool,    
}

impl<'input> Lexer<'input> {
    pub fn new(reader: &mut BufReader) -> Self {
        let linebuffer = String::new();
        let status = reader.read_line(linebuffer).unwrap();
        let line: &'input str = &linebuffer;
        Lexer { reader: reader, scaneol: false, lineno: 1, 
                chars: line.chars().peekable(), 
                first_column: 0, current_collumn: 0, needline: false
                
        }
    }
    include!(concat!(env!("OUT_DIR"), "/keywords.rs"));
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Tok, usize, LexicalError>;
    fn next(&mut self) -> Option<Self::Item> {
        fn IsDigit(ch: Option<char>) -> bool {
            match ch {
                Some('0'..='9') => true,
                _               => false,
            }
        }
        fn IsAlpha(ch: Option<char>) -> bool {
            match ch {
                Some('a'..='z') => true,
                Some('A'..='Z') => true,
                _               => false,
            }
        }
        loop {
            let mut word: String = String::new();
            if self.needline {
                self.lineno += 1;
                let linebuffer = String::new();
                let status = match self.reader.read_line(linebuffer) {
                    Ok(stat) => stat,
                    Err(message) return Some(Err(LexicalError::IOError(message.to_string))),
                };
                if status == 0 {return None;} // EOF
                let line: &'input str = &linebuffer;
                self.chars = line.chars().peekable();
                self.first_column = 0;
                self.current_collumn = 0;
                self.needline = false;
            }
            if self.scaneol {
                match self.chars.next() {
                    Some('\n') | None => {
                        self.scaneol = false;
                        let result Some(Ok((self.first_column, 
                                            Tok::STRINGTOEOL(word.clone()),
                                            self.current_column+1)));
                        self.first_column = self.current_column+1;
                        return result;                    
                    },
                    Some(ch) => {
                        word.push(ch.unwrap());
                        self.current_column += 1;
                    },
                };
            } else {
                let mut ch = self.chars.next();
                loop {
                    match ch {
                        Some(' ') | Some('\t') => {
                            ch = self.chars.next();
                            self.current_column += 1;
                        },
                        Some('\n') | None => {
                            let result =  Some(Ok((self.current_column,
                                                   Tok::EOL,
                                                   self.current_column+1)));
                            self.needline = true;
                            return result;
                        },
                        Some(any) => {break;},
                    };
                };
                if ch == Some('#') {
                    while self.chars.next().is_some() {
                        self.current_column += 1;
                    }
                    let result =  Some(Ok((self.current_column,
                                           Tok::EOL,
                                           self.current_column+1)));
                    self.needline = true;
                    return result;
                }
                if IsDigit(ch) ||
                    ((ch == Some('+') || ch == Some('-')) &&
                        IsDigit(self.chars.peek())) {
                    word.push(ch.unwrap());
                    self.current_column += 1;
                    ch = self.chars.next();
                    while IsDigit(ch) {
                        word.push(ch.unwrap());
                        ch = self.chars.next();
                        self.current_column += 1;
                    }
                    if ch == Some('.')
                    {
                        word.push(ch.unwrap());
                        ch = self.chars.next();
                        self.current_column += 1;
                        while IsDigit(ch) {
                            word.push(ch.unwrap());
                            ch = self.chars.next();
                            self.current_column += 1;
                        }
                        let f: f64 = f64::from_str(&word);
                        let result = Some(Ok((self.first_column,
                                              Tok::FLOAT(f),
                                              self.current_column+1)));
                        self.first_column = self.current_column+1;
                        return result;
                    }
                    let ui: u32 = u32::from_str(&word);
                    let result = Some(Ok((self.first_column,
                                          Tok::UINTEGER(ui),
                                          self.current_column+1)));
                    self.first_column = self.current_column+1;
                    return result;
                } else if IsAlpha(ch) {
                    while IsAlpha(ch) || ch == Some('$') {
                        word.push(ch.unwrap().to_ascii_uppercase());
                        ch = self.chars.next();
                        self.current_column += 1;
                    }
                    let firstColumn = self.first_column;
                    let lastColumn = self.current_column + 1;
                    let token = KEYWORDS.get(keyword).cloned();
                    if token.is_none() {
                        return Some(Err(LexicalError::UnknownKeyword));
                    } else {
                        return Some(Ok((firstColumn,token.unwrap(),lastColumn)));
                    }
                } else if ch == Some('"') {
                    let mut endOfString: bool = false;
                    ch = self.chare.next();
                    self.current_column += 1;
                    while !endOfString && ch != Some('\n') && !ch.is_none() {
                        if ch == Some('\\') {
                            ch = self.chare.next(); 
                            self.current_column += 1; 
                            word.push(ch,unwrap_or(' '));
                            ch = self.chare.next();
                            if ch.is_none() {
                                self.lineno += 1;
                                let linebuffer = String::new();
                                let status = match self.reader.read_line(linebuffer) {
                                    Ok(stat) => stat,
                                    Err(message) return Some(Err(LexicalError::IOError(message.to_string))),
                                };
                                if status == 0 {return None;} // EOF
                                let line: &'input str = &linebuffer;
                                self.chars = line.chars().peekable();
                                self.first_column = 0;
                                self.current_collumn = 0;
                                self.needline = false;
                            }
                        } else if ch == Some('"') {
                            if self.chars.peek() == Some('"') {
                                ch = self.chare.next();
                                self.current_collumn += 1;
                                word.push(ch.unwrap());
                            } else {
                                endOfString = true;
                            }
                        } else {
                            word.push(ch.unwrap());
                            self.current_collumn += 1;
                            ch = self.chare.next(); 
                        }
                    }
                    if ch != Some('"') {
                        return Some(Err(LexicalError::UnTerminatedString));
                    } else {
                        ch = self.chare.next();
                        self.current_collumn += 1; 
                        let firstCol = self.first_column;
                        let lastCol = self.current_collumn += 1;
                        self.first_column = lastCol + 1;
                        return Some(Ok((firstCol,Tok::STRING(word.clone()),lastCol)));
                    }
                } else {
                    if ch == Some('.') {
                        self.current_collumn += 1; 
                        let firstCol = self.first_column;
                        let lastCol = self.current_collumn;
                        self.first_column = lastCol+1;
                        ch = self.chare.next();
                        return Some(Ok((firstCol,Tok::DOT,lastCol)));
                    } else {
                    }
                }
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
        let mut file = match File::open(&layoutfilename) {
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
    }
    pub fn SetTitle(&mut self,level: u32, text: &str) {
    }
    pub fn SetMapscale(&mut self,mapscale: i32) {
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
