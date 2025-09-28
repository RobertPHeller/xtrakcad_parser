// -!- rust -!- //////////////////////////////////////////////////////////////
//
//  System        : 
//  Module        : 
//  Object Name   : $RCSfile$
//  Revision      : $Revision$
//  Date          : $Date$
//  Author        : $Author$
//  Created By    : Robert Heller
//  Created       : 2025-09-22 10:08:21
//  Last Modified : <250927.1402>
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
//use crate::Layout;
extern crate getopts;
use getopts::Options;
use std::env;
use xtrakcad_parser::Layout;

fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} FILE [options]", program);
    print!("{}", opts.usage(&brief));
}


fn main() {
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();

    let mut opts = Options::new();    
    opts.optflag("h", "help", "print this help menu");
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => { m },
        Err(f) => { panic!("{}", f.to_string());  },
    };
    if matches.opt_present("h") {
        print_usage(&program, opts);
        return ();
    };
    let layoutfile = if !matches.free.is_empty() {
        matches.free[0].clone()
    } else {
        print_usage(&program, opts);
        panic!("Missing layout file!");
    };
    let layout = match Layout::new(layoutfile) {
        Ok(l) => { l },
        Err(message) => { panic!("{}",message.to_string()); },
    };
    println!("{}",layout);
    for (index, layer) in layout.LayerIter() {
        println!("Layer # {}: {}",*index,*layer);
    }
    for (index, structure) in layout.StructureIter() {
        println!("Structure # {}: {}",*index,*structure);
    }
    for (index, drawing) in layout.DrawingIter() {
        println!("Drawing # {}: {}",*index,*drawing);
    }
    for (index, bzrline) in layout.BZRLineIter() {
        println!("BZRLine # {}: {}",*index,*bzrline);
    }
    for (index, cornu) in layout.CornuIter() {
        println!("Cornu # {}: {}",*index,*cornu);
    }
    for (index, curve) in layout.CurveIter() {
        println!("Curve # {}: {}",*index,*curve);
    }
    for (index, bezier) in layout.BezierIter() {
        println!("Bezier # {}: {}",*index,*bezier);
    }
    for (index, straight) in layout.StraightIter() {
        println!("Straight # {}: {}",*index,*straight);
    }
    for (index, turnout) in layout.TurnoutIter() {
        println!("Turnout # {}: {}",*index,*turnout);
    }
    for (index, turntable) in layout.TurntableIter() {
        println!("Turntable # {}: {}",*index,*turntable);
    }
    for (index, joint) in layout.JointIter() {
        println!("Joint # {}: {}",*index,*joint);
    }
    for (index, car) in layout.CarIter() {
        println!("Car # {}: {}",*index,*car);
    }
    for (index, note) in layout.NoteIter() {
        println!("Note # {}: {}",*index,*note);
    }
    for (index, textitem) in layout.TextItemIter() {
        println!("TextItem # {}: {}",*index,*textitem);
    }
    for (index, block) in layout.BlockIter() {
        println!("Block # {}: {}",*index,*block);
    }
    for (index, switchmotor) in layout.SwitchMotorIter() {
        println!("SwitchMotor # {}: {}",*index,*switchmotor);
    }
    for (index, signal) in layout.SignalIter() {
        println!("Signal # {}: {}",*index,*signal);
    }
    for (index, sensor) in layout.SensorIter() {
        println!("Sensor # {}: {}",*index,*sensor);
    }
    for (index, control) in layout.ControlIter() {
        println!("Control # {}: {}",*index,*control);
    }
}
