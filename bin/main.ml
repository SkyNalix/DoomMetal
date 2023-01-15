open AST
open Bogue
module W = Widget
module L = Layout
module T = Trigger
module A = Sdl_area
module S = Style

open Printf

(*
  doc de bogue: https://sanette.github.io/bogue/index_modules.html
  github: https://github.com/sanette/bogue
*)

let draw_area_widget = W.sdl_area ~w:500 ~h:500 
        ~style: (
            S.create 
              ~border:(S.mk_border ~radius:0 (Style.mk_line ~width:0 ())) () 
              ~background:(S.color_bg (Draw.opaque Draw.black))
        )
        ();;
let draw_area = W.get_sdl_area draw_area_widget;;
let level = ref (Level.empty ())
let height = ref 500;;
let width = ref 500;;
let label = W.label "temp string";;

let mouse_enter_action _ _ _ : unit =
    printf "mouse_enter_action\n" ;;

let mouse_motion_action _ : unit =
    let width= !width in
    let height = !height in
    let plot = !level.plot in
    let player_pos = !level.player.pos in
    let block_width = (width / (Array.length plot.(0))) in
    let block_height = (height / (Array.length plot)) in

    let (x,y) = Mouse.pos () in
    A.draw_line draw_area
        ~color:(Draw.opaque Draw.grey)
        ~thick:2
        (x,y)
        (int_of_float (float_of_int block_width *. player_pos.x),
            int_of_float (float_of_int block_height *. player_pos.y))
        ;
    
    let mouse_pos = {x=(float_of_int x) /. float_of_int block_width; y=float_of_int y /. float_of_int block_height} in
    let (tileFound, distance, touched_pos) = 
        Raycasting.raycast !level mouse_pos in

    if tileFound then (
        A.fill_rectangle draw_area 
            ~color:(Draw.opaque Draw.black)
            ~w:(block_width)
            ~h:(block_height)
            ((int_of_float touched_pos.x)*block_width, (int_of_float touched_pos.y)*block_height);
        Sdl_area.update draw_area;
        W.update draw_area_widget;
        W.update label;
    )
    ;;



let update_player_pos (x:float) (y:float) : unit =
    let lev = !level in
    level := {lev with 
        player={ lev.player with
            pos={
                x=(lev.player.pos.x +. x);
                y=(lev.player.pos.y +. y)
            }
        }
    };
    ()
    
let key_down_action _ _ _ : unit = 
    let open Tsdl.Sdl in
    let keystates = get_keyboard_state () in
    if keystates.{get_scancode_from_key (K.z)} <> 0 then (
        update_player_pos 0. (-0.2);
    );
    if keystates.{get_scancode_from_key (K.s)} <> 0 then (
        update_player_pos 0. 0.2;
    );
    if keystates.{get_scancode_from_key (K.d)} <> 0 then (
        update_player_pos 0.2 0.;
    );
    if keystates.{get_scancode_from_key (K.q)} <> 0 then (
        update_player_pos (-0.2) 0.;
    );
    ();;

(*
  Liste d'events : https://sanette.github.io/bogue/Bogue.Trigger.html
            et : https://ocaml.org/p/tsdl/0.9.6/doc/Tsdl/Sdl/index.html#type-event_type   
 *)


let event_action f a b c= 
 f a b c;
 Drawer.drawLevel draw_area !level !height !width;
 Sdl_area.update draw_area;
 W.update draw_area_widget

let bind_draw_area_events () : W.connection list =   
    W.on_click draw_area_widget ~click:mouse_motion_action;
    W.connect draw_area_widget label (event_action mouse_enter_action) [T.mouse_enter]
    :: W.connect draw_area_widget label (event_action key_down_action) [T.key_down]
    :: [];;


let main () =

    Drawer.drawLevel draw_area !level !height !width ;
    let layout = L.flat_of_w ~scale_content:false [draw_area_widget] in
    let events = bind_draw_area_events () in
    let board = Bogue.of_layout ~connections:events layout in
    Bogue.run board ;;

let () = 
    main ();
    Bogue.quit ()