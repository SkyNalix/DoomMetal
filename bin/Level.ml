open AST
let empty () : level = {
  player = {
     pos = {x=4.5; y=4.5};
     view_angle = 0
  };
  plot=Plot.empty ()
};;