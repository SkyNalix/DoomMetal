type tile = 

  | NOTHING
  | WALL
  | RED_WALL
  | TRANSPARENT_WALL

type plot = tile array array

type position = { mutable x : float; mutable y : float }

type player = {
  pos : position;
  view_angle : int;
  mutable hp : int;
}

type enemy = {
   mutable pos : position;
   mutable nom : string;
   mutable hp : int ;
}  

type level = { 
  mutable player : player; 
  mutable enemies : enemy list;
  plot : plot;
  plot_height : float;
  plot_width : float;
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
    block_height : int;
    block_width : int;
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
