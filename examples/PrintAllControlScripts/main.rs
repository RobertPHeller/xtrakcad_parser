// -!- rust -!- //////////////////////////////////////////////////////////////
//
//  System        : 
//  Module        : 
//  Object Name   : $RCSfile$
//  Revision      : $Revision$
//  Date          : $Date$
//  Author        : $Author$
//  Created By    : Robert Heller
//  Created       : 2025-09-28 13:09:45
//  Last Modified : <250928.1415>
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

//! This example program prints out all of the layout control elements (Blocks,
//! SwitchMotors, Signals, Sensors, and Controls.
//!
//! ## Synopsis
//!
//! PrintAllControlScripts FILE \[options\]
//!
//! ## Options
//! - -b, --blocks print Blocks
//! - -m, --switchmotors print Switchmotors
//! - -s, --signals print Signals
//! - -c, --controls print Controls
//! - -x, --sensors print Sensors
//! - -a, --all print all (this is the default)
//! - -h, --help print help
//! ## Parameters
//!
//! The layout file to load.
//!
//! ## Description
//!
//! This program loads in a layout file and prints the selected control 
//! elements.
//!
//! ## Author
//! Robert Heller \<heller@deepsoft.com\>


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
    opts.optflag("b", "blocks", "print Blocks");
    opts.optflag("m", "switchmotors", "print Switchmotors");
    opts.optflag("s", "signals", "print Signals ");
    opts.optflag("c", "controls", "print Controls");
    opts.optflag("x", "sensors", "print Sensors");
    opts.optflag("a", "all", "print all (this is the default)");
    opts.optflag("h", "help", "print this help menu");
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => { m },
        Err(f) => { panic!("{}", f.to_string());  },
    };
    if matches.opt_present("h") {
        print_usage(&program, opts);
        return ();
    };
    let mut blocks: bool = false;
    let mut switchmotors: bool = false;
    let mut signals: bool = false;
    let mut controls: bool = false;
    let mut sensors: bool = false;
    let mut all: bool = true;
    if matches.opt_present("b") {
        blocks = true;
        all = false;
    }
    if matches.opt_present("m") {
        switchmotors = true;
        all = false;
    }
    if matches.opt_present("s") {
        signals = true; 
        all = false;
    }
    if matches.opt_present("c") {
        controls = true; 
        all = false;
    }
    if matches.opt_present("x") {
        sensors = true;
        all = false;
    }
    if matches.opt_present("a") {
        all = true;
    }
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
    if blocks || all {
        for (index, block) in layout.BlockIter() {
            println!("Block # {}:",*index);
            println!("\tName: {}",block.Name());
            println!("\tScript: {}",block.Script());
            println!("\tTrack list: {}",block.Tracklist());
        }
    }
    if switchmotors || all {
        for (index, switchmotor) in layout.SwitchMotorIter() {
            println!("SwitchMotor # {}:",*index);
            println!("\tName: {}",switchmotor.Name());
            let turnout = layout.Turnout(switchmotor.Turnout()).expect("Should not happen");
            let tablist = turnout.Tablist();
            let tabelts: Vec<&str> = tablist.split('\t').collect();
            println!("\tTurnout #{} ({})",switchmotor.Turnout(),tabelts[1]);
            println!("\tNormal: {}",switchmotor.Normal());
            println!("\tReverse: {}",switchmotor.Reverse());
            println!("\tPoint Sense: {}",switchmotor.Pointsense());
        }
    }
    if signals || all {
        for (index, signal) in layout.SignalIter() {
            println!("Signal # {}:",*index);
            println!("\tName: {}",signal.Name());
            println!("\tHeads: {}",signal.Numheads());
            let aspects = signal.Aspectlist();
            if aspects.len() > 0 {
                println!("\tAspects:");
                for ia in 0..aspects.len() {
                    let aspect = &aspects[ia];
                    println!("\t\t{}: {}",aspect.Name(), aspect.Script());
                }
            }
        }
    }
    if sensors || all {
        for (index, sensor) in layout.SensorIter() {
            println!("Sensor # {}",*index);
            println!("\tName: {}",sensor.Name());
            println!("\tScript: {}",sensor.Script());
        }
    }
    if controls  || all {
        for (index, control) in layout.ControlIter() {
            println!("Control # {}",*index);
            println!("\tName: {}",control.Name());
            println!("\tOn Script: {}",control.OnScript());
            println!("\tOff Script: {}",control.OffScript());
        }
    }
}
        
