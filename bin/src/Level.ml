open AST
let get s = 
  let pos, plot = Plot.parsePlot s in
  {
  player = {
     pos = pos;
     view_angle = 180;
     hp = 100
  };
  plot=plot
};;