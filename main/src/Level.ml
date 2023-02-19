open AST
let get s = 
  let pos, plot = Plot.parsePlot s in
  {
  player = {
     pos = pos;
     view_angle = 180
  };
  plot=plot;
  plot_height = float_of_int (Array.length plot);
  plot_width = float_of_int (Array.length plot.(0));
};;