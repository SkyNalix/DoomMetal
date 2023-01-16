open AST
open Bogue
module W = Widget
module L = Layout
module T = Trigger
module A = Sdl_area
module S = Style

    (*
      Liste d'events : https://sanette.github.io/bogue/Bogue.Trigger.html
                et : https://ocaml.org/p/tsdl/0.9.6/doc/Tsdl/Sdl/index.html#type-event_type   
     *)
let bind_default_events windows_info level  = (

    let update_player (x:float) (y:float) (view_angle:int): unit =
        level.player <- {
            pos = {
                x=(level.player.pos.x +. x);
                y=(level.player.pos.y +. y)
            };
            view_angle = (level.player.view_angle + view_angle) mod 360
        }; () in

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
        () in


    let update_after_action f a b c = 
        A.clear windows_info.draw_area; 
        f a b c;
        Drawer2D.drawLevel windows_info level;
        Raycasting.raycast windows_info level;

        Sdl_area.update windows_info.draw_area;
        W.update windows_info.draw_area_widget in

    W.connect windows_info.draw_area_widget windows_info.label (update_after_action key_down_action) [T.key_down]
    :: []
);;
  

let make_default_windows_info () : windows_info = (
    let draw_area_widget = W.sdl_area ~w:500 ~h:500 
        ~style: (
            S.create 
              ~border:(S.mk_border ~radius:0 (Style.mk_line ~width:0 ())) () 
              ~background:(S.color_bg (Draw.opaque Draw.black))
        )
        () in

    {
        draw_area_widget = draw_area_widget;
        draw_area = W.get_sdl_area draw_area_widget;
        level = Level.empty ();
        height = 500 ;
        width = 500 ;
        label = W.label "temp string"
    }
)