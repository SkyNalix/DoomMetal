
type tile = 
  | WALL
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
