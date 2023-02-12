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


type parameters =  {
  debug : bool;
  tmp : bool (* ajouté pour eviter des warnings, a enlever apres ajout d'autres parametres *)
}

type windows_info = {
    parameters : parameters;
    window : Sdlwindow.t;
    render : Sdltype.renderer;
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
  angle_step : int
}
