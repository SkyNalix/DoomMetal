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

    let update_player x y view_angle =
        level.player <- {
            pos = {
                x=(level.player.pos.x +. x);
                y=(level.player.pos.y +. y)
            };
            view_angle = (level.player.view_angle + view_angle) mod 360
        }; () in

    let key_down_action _ _ _ = 
        let open Tsdl.Sdl in
        let radians = (float_of_int level.player.view_angle) *. (Float.pi /. 180.) in
        let x_change = sin radians *. 0.1 in
        let y_change = cos radians *. 0.1 in

        let keystates = get_keyboard_state () in
        if keystates.{get_scancode_from_key (K.z)} <> 0 then (
            update_player x_change y_change 0;
        );
        if keystates.{get_scancode_from_key (K.s)} <> 0 then (
            update_player (Float.neg x_change) (Float.neg y_change) 0;
        );
        if keystates.{get_scancode_from_key (K.d)} <> 0 then (
            update_player (Float.neg y_change) x_change 0;
        );
        if keystates.{get_scancode_from_key (K.q)} <> 0 then (
            update_player y_change (Float.neg x_change) 0;
        );

        if keystates.{get_scancode_from_key (K.right)} <> 0 then (
            update_player 0. 0. (-15);
        );
        if keystates.{get_scancode_from_key (K.left)} <> 0 then (
            update_player 0. 0. 15;
        );
        () in

    let update_after_action f a b c = 
        A.clear windows_info.area3D; 
        A.clear windows_info.area2D; 
        f a b c;
        Raycasting.raycast windows_info level;
        Sdl_area.update windows_info.area3D;
        Sdl_area.update windows_info.area2D;
        W.update windows_info.area3D_widget;
        W.update windows_info.area2D_widget in

    W.connect windows_info.area3D_widget windows_info.area2D_widget 
                (update_after_action key_down_action) [T.key_down]
    :: W.connect windows_info.area3D_widget windows_info.area2D_widget 
                (update_after_action (fun _ _ _ -> ()))  [Tsdl.Sdl.Event.mouse_motion]
    :: []
);;
  

let make_default_windows_info () : windows_info = (
    let makearea () = W.sdl_area ~w:500 ~h:500 
        ~style: (
            S.create 
              ~border:(S.mk_border ~radius:0 (Style.mk_line ~width:0 ())) () 
              ~background:(S.color_bg (Draw.opaque Draw.black))
        )
        () in
    let area3D_widget = makearea () in
    let area2D_widget = makearea () in
    let level = Level.empty () in
    {
        area3D_widget = area3D_widget;
        area3D = W.get_sdl_area area3D_widget;
        area2D_widget = area2D_widget;
        area2D = W.get_sdl_area area2D_widget;
        level = level;
        height = 500 ;
        width = 500 ;
        block_width = 500 / (Array.length level.plot.(0));
        block_height = 500 / (Array.length level.plot);
    }
)