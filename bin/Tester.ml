open AST
open Bogue
module W = Widget
module L = Layout
module T = Trigger
module A = Sdl_area
module S = Style

let raycast windows_info = (
    let level = Level.empty () in
    Drawer2D.drawLevel windows_info level ;
    let layout = L.flat_of_w ~scale_content:false [windows_info.draw_area_widget] in
    let events = Common.bind_default_events windows_info level in
    let board = Bogue.of_layout ~connections:events layout in
    Bogue.run board
);;


let run_test windows_info =
    match Sys.argv.(1) with
    | "raycast" -> raycast windows_info
    | _ -> ();;