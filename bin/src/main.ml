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


let main () =
    let level = Level.get "empty_level" in
    let windows_info = Common.make_default_windows_info level in
    let layout = L.flat_of_w 
        ~background:(L.opaque_bg Draw.grey)
        [windows_info.area3D_widget; windows_info.area2D_widget] in
    Raycasting.raycast windows_info level;
    let events = Common.bind_default_events windows_info level in
    let board = Bogue.of_layout ~connections:events layout in
    Bogue.run board;
    ();;

let () = 
    main ();
    Bogue.quit ();
    ();;