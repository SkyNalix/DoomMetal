open Bogue

type tile = 
  | NOTHING
  | WALL
  | RED_WALL
  | TRANSPARENT_WALL

type plot = tile array array

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
    area3D_widget : Widget.t;
    area3D : Sdl_area.t;
    area2D_widget : Widget.t;
    area2D : Sdl_area.t;
    mutable height : int ;
    mutable width : int ;
    mutable block_height : int;
    mutable block_width : int;
}

type ray = {
  rayTouched : bool;
  distance: float; 
  touched_pos : position;
  intersection : position;
  angle : int;
  angle_vec : position; (*vecteur de l'angle où le joueur regarde*)
  angle_min : int;
  angle_max : int;
}