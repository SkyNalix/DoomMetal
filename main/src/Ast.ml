type tile = 
  | NOTHING
  | WALL
  | RED_WALL
  | TRANSPARENT_WALL

type floor_tile = 
  | NORMAL
  | ICE

type position = { mutable x : float; mutable y : float }

type player = {
  pos : position;
  mutable view_angle : int;
  mutable hp : int;
  mutable forward_speed : float;
  mutable sideway_speed : float;
}

type enemy = {
   mutable pos : position;
   mutable hp : int ;
}  

type map = {
  ceiling : bool;
  plot : tile array array;
  floor : floor_tile array array;
  height : int;
  width : int;
}

type level = { 
  mutable player : player; 
  mutable enemies : enemy list;
  map : map
}

type parameters =  {
  debug : bool;
  drawer2D : bool
}

type windows_info = {
    parameters : parameters;
    window : Sdlwindow.t;
    render : Sdltype.renderer;
    height : int ;
    width : int ;
    drawer3D_height : int;
    drawer3D_width : int;
    drawer2D_height : int;
    drawer2D_width : int;
}

type ray = {
  rayTouched : bool;
  distance: float; 
  touched_pos : position;
  intersection : position;
  angle : float;
  angle_vec : position; (*vecteur de l'angle où le joueur regarde*)
  angle_min : float;
  angle_max : float;
  angle_step : float
}
