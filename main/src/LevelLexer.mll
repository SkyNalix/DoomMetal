{
open LevelParser
}

let ignore   = '\010' | '\013' | "\013\010" | " " | "\009" | "\012"

let int = [ '0'-'9' ]+
let float =  [ '0'-'9' ]+ "." [ '0'-'9' ]+

rule main = parse
  | ignore+         { main lexbuf }
  | "level"         { LEVEL }
  | "player"        { PLAYER }
  | "pos"       	  { POS }
  | "view_angle"    { VIEW_ANGLE }
  | "hp"       	    { HP }
  | "plot"          { PLOT }
  | "enemies"       { ENEMIES }
  | "enemy"         { ENEMY }
  | "map"           { MAP }
  | "ceiling"       { CEILING }
  | "floor"         { FLOOR }
  | "fov"           { FOV }
  | "="             { EQ }
  | "x"             { X }
  | "y"             { Y }
  | ';'			        { SEMICOLON }
  | '{'			        { LBRACKET }
  | '}'			        { RBRACKET }
  | float as f      { FLOAT (float_of_string f) }
  | int as n	      { INT (int_of_string n) }
  | eof					    { EOF }
  | _               { failwith "unexpected character." }
