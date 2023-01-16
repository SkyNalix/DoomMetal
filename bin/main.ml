open Bogue
module W = Widget
module L = Layout
module T = Trigger
module A = Sdl_area
module S = Style

(*
  doc de bogue: https://sanette.github.io/bogue/index_modules.html
  github: https://github.com/sanette/bogue
*)

let windows_info = Common.make_default_windows_info ();;

let main () =
    (* let level = Level.empty () in
    Drawer2D.drawLevel windows_info level ;
    let layout = L.flat_of_w ~scale_content:false [windows_info.draw_area_widget] in
    let events = Common.bind_default_events windows_info level in
    let board = Bogue.of_layout ~connections:events layout in
    Bogue.run board; *)
    ();;

let () = 
    if Array.length Sys.argv > 1 then (
        Array.iter (fun v -> Printf.printf "%s\n" v) Sys.argv;
        Tester.run_test windows_info
    ) else (
        main ();
        Bogue.quit ()
    );;