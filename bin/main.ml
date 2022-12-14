open Printf

let plot = [
  [1;1;1;1;1;1;1];
  [1;0;0;0;0;0;1];
  [1;0;0;0;0;0;1];
  [1;0;0;0;0;0;1];
  [1;0;0;0;0;0;1];
  [1;0;0;0;0;0;1];
  [1;1;1;1;1;1;1];
]

let rec drawPlot plot : unit =
  let rec drawSquare row height width =
    match row with 
      | [] -> ()
      | element::rest -> 

          drawSquare rest height (width+1)
  in
  let rec drawRow plot height =  
    match plot with
      | [] -> ()
      | row::rest -> 
          ignore (drawSquare row height 0);
          drawRow rest (height+1)
  in
  drawRow plot 0 
  





(*
  doc de bogue: https://sanette.github.io/bogue/index_modules.html
  github: https://github.com/sanette/bogue
*)

open Bogue

module W = Widget
module L = Layout
module T = Trigger
module A = Sdl_area

let mouse_enter_action (draw_area_widget: W.t) _ (event : Tsdl.Sdl.event) : unit =
  printf "mouse_enter_action\n" 

let mouse_motion_action (draw_area_widget: W.t) _ (event : Tsdl.Sdl.event) : unit =
  printf "mouse_motion_action\n" 


(*
  Liste d'events : https://sanette.github.io/bogue/Bogue.Trigger.html
            et : https://ocaml.org/p/tsdl/0.9.6/doc/Tsdl/Sdl/index.html#type-event_type   
 *)
let bind_draw_area_events (draw_area_widget: W.t) : W.connection list =   
  let label = W.label "" in
  W.connect draw_area_widget label mouse_enter_action [T.mouse_enter]
  (* :: W.connect draw_area_widget label mouse_motion_action [Tsdl.Sdl.Event.mouse_motion] *)
  :: []

let draw (area: A.t) : unit = 
  A.draw_rectangle area ~color:(Draw.opaque Draw.red) ~thick:20 ~w:300 ~h:300 (0, 0); 
  ()

let main () =

  let draw_area_widget = W.sdl_area 500 500 () in
  let draw_area = W.get_sdl_area draw_area_widget in

  draw draw_area;

  let events = bind_draw_area_events draw_area_widget in

  let layout = L.flat_of_w ~scale_content:false [draw_area_widget] in
  let board = Bogue.make events [layout] in
  (* Layout.on_resize layout (fun () -> 
      let (h,w) = Layout.get_size layout in  Window.set_size h w board); *)
  Bogue.run board

let () = main ();
  Bogue.quit ()