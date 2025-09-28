# XTrackCAD layout file parser

Ported from the Model RR System's Bision++ / C++ XTrackCAD layout file parser.

The parser is invoked as part of the new() class function of the Layout 
struct. This function takes a layout filename as its only parameter. It
opens the file and procedes to parse the file and the parser's action code 
populates the Layout struct with the contents of the file.  The new()
returns the fully populated struct, which can then be accessed via various
accessor methods to retrieve information about the layout, including 
tracks, structures, scenery features, and control features.
