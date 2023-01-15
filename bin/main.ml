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
let level = Level.empty ()
let height = ref 500;;
let width = ref 500;;
let label = W.label "temp string";;



let raycast () : unit =
    let width= !width in
    let height = !height in
    let player = level.player in
    let block_width = (width / (Array.length level.plot.(0))) in
    let block_height = (height / (Array.length level.plot)) in


    let player_posX = int_of_float (float_of_int block_width *. player.pos.x) in
    let player_posY = int_of_float (float_of_int block_height *. player.pos.y) in

    let radians = (float_of_int player.view_angle) *. (Float.pi /. 180.) in
    let viewVect = { x=sin radians; y=cos radians} in
    viewVect.x <- (viewVect.x *. (float_of_int width));
    viewVect.y <- (viewVect.y *. (float_of_int height));

    A.draw_line draw_area
        ~color:(Draw.opaque Draw.grey)
        ~thick:2
        (int_of_float viewVect.x, int_of_float viewVect.y)
        (player_posX, player_posY);
    
    let mouse_pos = {x=viewVect.x /. float_of_int block_width; y=viewVect.y /. float_of_int block_height} in
    let (tileFound, distance, touched_pos) = 
        Raycasting.raycast level mouse_pos in

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

let update_player (x:float) (y:float) (view_angle:int): unit =
    level.player <- {
        pos = {
            x=(level.player.pos.x +. x);
            y=(level.player.pos.y +. y)
        };
        view_angle = (level.player.view_angle + view_angle) mod 360
    };
    ()
    
let key_down_action _ _ _ : unit = 
    let open Tsdl.Sdl in
    let keystates = get_keyboard_state () in
    if keystates.{get_scancode_from_key (K.z)} <> 0 then (
        update_player 0. (-0.1) 0;
    );
    if keystates.{get_scancode_from_key (K.s)} <> 0 then (
        update_player 0. 0.1 0;
    );
    if keystates.{get_scancode_from_key (K.d)} <> 0 then (
        update_player 0.1 0. 0;
    );
    if keystates.{get_scancode_from_key (K.q)} <> 0 then (
        update_player (-0.1) 0. 0;
    );
    if keystates.{get_scancode_from_key (K.right)} <> 0 then (
        update_player 0. 0. (-15);
    );
    if keystates.{get_scancode_from_key (K.left)} <> 0 then (
        update_player 0. 0. 15;
    );
    ();;

(*
  Liste d'events : https://sanette.github.io/bogue/Bogue.Trigger.html
            et : https://ocaml.org/p/tsdl/0.9.6/doc/Tsdl/Sdl/index.html#type-event_type   
 *)

let update_after_action f a b c = 
    A.clear draw_area; 
    f a b c;
    Drawer.drawLevel draw_area level !height !width;
    raycast ();

    Sdl_area.update draw_area;
    W.update draw_area_widget



let bind_draw_area_events () : W.connection list =  
    W.connect draw_area_widget label (update_after_action key_down_action) [T.key_down]
    :: [];;


let main () =
    Drawer.drawLevel draw_area level !height !width ;
    let layout = L.flat_of_w ~scale_content:false [draw_area_widget] in
    let events = bind_draw_area_events () in
    let board = Bogue.of_layout ~connections:events layout in
    Bogue.run board ;;

let () = 
    main ();
    Bogue.quit ()