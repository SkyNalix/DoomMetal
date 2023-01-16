open Bogue

type tile = 
  | WALL
  | REDWALL
  | NOTHING

type plot = tile array array

type intPosition = { mutable x : int; mutable y : int }
type position = { mutable x : float; mutable y : float }

type player = {
  pos : position;
  view_angle : int
}

type level = { 
  mutable player : player; 
  mutable plot : plot
}


type windows_info = {
    draw_area_widget : Widget.t;
    draw_area : Sdl_area.t;
    level : level;
    mutable height : int ;
    mutable width : int ;
    label : Widget.t
}