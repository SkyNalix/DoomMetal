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

let myIntToFloat x = (* regarde si on doit arondir au supérieur ou inférieur DANS CERTAIN CAS pour le déplacement*)
    let y = int_of_float(x) in 
    
    let z = x -. float_of_int(y) in 
    if z >= 0.05 then y+1 else if z <= -0.1 then y-1 else y 

;;

let floatToInt x = 
    let sub = int_of_float(x) in 
    if (x -. float_of_int(sub) >= 0.5 ) then sub+1 else sub 
;;

let print_Position (pos : position)  (str : string) =
    print_string(str );
    print_float(pos.x);
    print_string("    ");
    print_float(pos.y);
    print_string(" \n");
    ()
;;

let arrondir (x : float) (valeur : float ) =  (* -1 -0.6    *)
    x +. 0.35 > valeur && x -. 0.35 < valeur
;;

let toucheUnMur x y (level : level)=

    
    let tx = floatToInt x in 
    let ty = floatToInt y in 
    print_string("what's happening here ? : ") ; print_int( floatToInt x); print_string("\n");
    
    let tuile = level.plot.(tx).(ty) in 
    match tuile with 
    | NOTHING -> print_string("NOTHING \n"); false 
    | _ -> print_string("MUR \n"); true
;;

let rec auxTire  y_change x_change (player : player) moblist py px level = (* mettre le x_change aussi*)
        
        
        let rec aux player liste  =
                match liste with
                | enn  :: l ->  if (arrondir enn.posE.y py && arrondir enn.posE.x px   ) then (print_string("TOUCHER \n"); 
                        let aa : int ref = enn.hp in enn.hp := !aa -1; Drawer2D.moblist := List.filter (fun enn -> enn.hp > ref 0) !Drawer2D.moblist ; 
                
                        () ) 
                                else (*print_Position enn.posE  "Ennemi :"; print_float(py); print_string("\n"); *) aux player l

                | [] -> ()
        in
        
        print_float(px);
        print_string(" ");
        print_float(py);
        print_string(" \n");

        if (py < 8. && py > 0. && px > 0. && px < 8. && (toucheUnMur px py level) =false ) then (   aux player moblist ; auxTire y_change x_change  player moblist (py +. y_change) (px +. x_change) level )else(
        (*print_Position player.pos "player : "; *) 

        print_string(" \n FIN METHODE \n");   )

;;

let aporte (player:player) (ennemi : ennemi) = 
    if ( player.pos.x >= ennemi.posE.x && player.pos.x <= ennemi.posE.x +. 1.5 
        && player.pos.y >= ennemi.posE.y && player.pos.y <= ennemi.posE.y +. 1.5) then true 
    else( if( player.pos.x <= ennemi.posE.x && player.pos.x >= ennemi.posE.x -. 1.5 
        && player.pos.y <= ennemi.posE.y && player.pos.y >= ennemi.posE.y -. 1.5
    )    then true 
    else (if (player.pos.x >= ennemi.posE.x && player.pos.x <= ennemi.posE.x +. 1.5 &&
        player.pos.y <= ennemi.posE.y && player.pos.y >= ennemi.posE.y -. 1.5 ) then true  
    else (if (player.pos.x <= ennemi.posE.x && player.pos.x >= ennemi.posE.x -. 1.5 && 
        player.pos.y >= ennemi.posE.y && player.pos.y <= ennemi.posE.y +. 1.5) then true else false   
    ) ) ) 
;;

let deplacement (player:player) (ennemi : ennemi)  = (* Deplacement float = 0.05, déplacement map = 1 *)

        if(player.pos.x >= ennemi.posE.x) then( 
            ennemi.posE <- {
                x = ennemi.posE.x +. 0.08;
                y = ennemi.posE.y;
            }; )
            else ( 
                ennemi.posE <- {
                x = ennemi.posE.x -. 0.08;
                y = ennemi.posE.y;
            };
            ) ;

        if(player.pos.y >= ennemi.posE.y) then (
            ennemi.posE <- {
                x = ennemi.posE.x;
                y = ennemi.posE.y +. 0.08;
            };
         )     
        else (
            ennemi.posE <- {
                x = ennemi.posE.x;
                y = ennemi.posE.y -. 0.08;
            };
        ) ;
            
            
        print_float(player.pos.x);
        print_string(" | ");
        print_float(player.pos.x);
        print_string(" \n");
    
        print_float(ennemi.posE.x);
        print_string(" ");
        print_float(ennemi.posE.x);
        print_string(" \n");


            ()

;;


let actionEnnemi (player : player) = 
    
    let see_Player player = (* Faire en sorte que les mob réagissent en cas de tir*)
        
        let rec aux liste = 
            match liste with
            | [] -> ()
            | x :: l ->  
                    if (aporte player x) then (print_string("deplacement \n"); deplacement player x; () )
                    else print_string("pas vue \n"); ()
            in 

        aux !Drawer2D.moblist            
    in

    see_Player player;
;;

let bind_default_events windows_info level  = (

    let update_player x y view_angle =
        let a = myIntToFloat (level.player.pos.x +. x ) -1 in 
        let b = myIntToFloat (level.player.pos.y +. y  ) -1 in
        let tuile = level.plot.(a).(b) in 


        match tuile with 
        | RED_WALL -> ()
        | NOTHING -> level.player <- {
            pos = {
                x=(level.player.pos.x +. x);
                y=(level.player.pos.y +. y)
            };
            view_angle = (level.player.view_angle + view_angle) mod 360
        }; ()
        |_ ->  ()
        

        in


    let key_down_action _ _ _ = 
        let open Tsdl.Sdl in
        let radians = (float_of_int level.player.view_angle) *. (Float.pi /. 180.) in
        let x_change = sin radians *. 0.1 in
        let y_change = cos radians *. 0.1 in 
        actionEnnemi level.player; 

        let keystates = get_keyboard_state () in
        if keystates.{get_scancode_from_key (K.z)} <> 0 then (

            update_player x_change y_change 0
        );
        if keystates.{get_scancode_from_key (K.s)} <> 0 then (

            update_player (Float.neg x_change) (Float.neg y_change) 0;
        );
        if keystates.{get_scancode_from_key (K.d)} <> 0 then (


            update_player (Float.neg y_change) x_change  0;
        );
        if keystates.{get_scancode_from_key (K.q)} <> 0 then (

            update_player  y_change (Float.neg x_change) 0 ;
        );

        if keystates.{get_scancode_from_key (K.right)} <> 0 then (
            update_player 0. 0. (-15);
        );
        if keystates.{get_scancode_from_key (K.left)} <> 0 then (
            update_player 0. 0. 15;
        );
        if keystates.{get_scancode_from_key (K.space)} <> 0 then (
            print_string("tire \n");
            
            auxTire y_change x_change level.player !Drawer2D.moblist level.player.pos.y level.player.pos.x level
        );
        () in

        (*let ma_direction x y angle =
            if angle <45 && angle > 0 
        in *)

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


  

let make_default_windows_info level : windows_info = (     

    let makearea () = W.sdl_area ~w:500 ~h:500 
        ~style: (
            S.create 
              ~border:(S.mk_border ~radius:0 (Style.mk_line ~width:0 ())) () 
              ~background:(S.color_bg (Draw.opaque Draw.black))
        )
        () in
    let area3D_widget = makearea () in
    let area2D_widget = makearea () in
    {
        area3D_widget = area3D_widget;
        area3D = W.get_sdl_area area3D_widget;
        area2D_widget = area2D_widget;
        area2D = W.get_sdl_area area2D_widget;
        height = 500 ;
        width = 500 ;
        block_width = 500 / (Array.length level.plot.(0));
        block_height = 500 / (Array.length level.plot);
    }
)