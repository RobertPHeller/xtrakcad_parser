// -!- rust -!- //////////////////////////////////////////////////////////////
//
//  System        : 
//  Module        : 
//  Object Name   : $RCSfile$
//  Revision      : $Revision$
//  Date          : $Date$
//  Author        : $Author$
//  Created By    : Robert Heller
//  Created       : 2025-09-21 09:55:38
//  Last Modified : <250922.2100>
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
use std::env;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;

fn main() {
    let path = Path::new(&env::var("OUT_DIR").unwrap()).join("keywords.rs");
    let mut file = BufWriter::new(File::create(&path).unwrap());

    write!(
        &mut file,
        "static KEYWORDS: phf::Map<&'static str, Tok> = {}",
        phf_codegen::Map::new()
            .entry("A", "Tok::A")
            .entry("ADJUSTABLE", "Tok::ADJUSTABLE")
            .entry("ASPECT", "Tok::ASPECT")
            .entry("B", "Tok::B")
            .entry("BEZIER", "Tok::BEZIER")
            .entry("BLOCK", "Tok::BLOCK")
            .entry("BZRLIN", "Tok::BZRLIN")
            .entry("C", "Tok::C")
            .entry("CAR", "Tok::CAR")
            .entry("CONTROL", "Tok::CONTROL")
            .entry("CORNU", "Tok::CORNU")
            .entry("CURRENT", "Tok::CURRENT")
            .entry("CURVE", "Tok::CURVE ")
            .entry("D", "Tok::D")
            .entry("DRAW", "Tok::DRAW ")
            .entry("E", "Tok::E")
            .entry("END$BLOCK", "Tok::ENDBLOCK")
            .entry("END$SEGS", "Tok::ENDSEGS")
            .entry("END$SIGNAL", "Tok::ENDSIGNAL")
            .entry("END$TRACKS", "Tok::ENDTRACKS")
            .entry("F", "Tok::F")
            .entry("G", "Tok::G")
            .entry("H", "Tok::H")
            .entry("HO", "Tok::HO")
            .entry("J", "Tok::J")
            .entry("JOINT", "Tok::JOINT")
            .entry("L", "Tok::L")
            .entry("LAYERS", "Tok::LAYERS ")
            .entry("M", "Tok::M")
            .entry("MAIN", "Tok::MAIN")
            .entry("MAPSCALE", "Tok::MAPSCALE")
            .entry("N", "Tok::N ")
            .entry("NOTE", "Tok::NOTE")
            .entry("O", "Tok::O")
            .entry("P", "Tok::P")
            .entry("PIER", "Tok::PIER")
            .entry("Q", "Tok::Q")
            .entry("ROOMSIZE", "Tok::ROOMSIZE")
            .entry("S", "Tok::S")
            .entry("SCALE", "Tok::SCALE")
            .entry("SENSOR", "Tok::SENSOR")
            .entry("SIGNAL", "Tok::SIGNAL")
            .entry("STRAIGHT", "Tok::STRAIGHT")
            .entry("STRUCTURE", "Tok::STRUCTURE")
            .entry("SUBSEGS", "Tok::SUBSEGS")
            .entry("SUBSEND", "Tok::SUBSEND")
            .entry("SWITCHMOTOR", "Tok::SWITCHMOTOR")
            .entry("T", "Tok::T")
            .entry("TEXT", "Tok::TEXT")
            .entry("TITLE", "Tok::TITLE")
            .entry("TRK", "Tok::TRK")
            .entry("TURNOUT", "Tok::TURNOUT")
            .entry("TURNTABLE", "Tok::TURNTABLE")
            .entry("VERSION", "Tok::VERSION")
            .entry("W", "Tok::W")
            .entry("X", "Tok::X")
            .entry("Y", "Tok::Y")
            .entry("Z", "Tok::Z")
            .build()
    ).unwrap();
    write!(&mut file, ";\n").unwrap();
    lalrpop::process_src().unwrap();
}
