open Ast


let texture_to_plot_tile textures = function
| WALL -> List.assoc "white_bricks" textures
| RED_WALL -> List.assoc "red_bricks" textures
| NOTHING -> failwith "no texture for NOTHING"
| TRANSPARENT_WALL ->  failwith "no texture for TRANSPARENT_WALL"

(*let reload = ref false ;; *)
let sec_fin = ref 0.0;;

let float_approx_eq f1 f2 approx = 
    f1 < (f2+.approx) && f1 > (f2-.approx)

let drawRay windows_info level textures ray =
    if not (ray.rayTouched) then (failwith "ray not touched") else
    
    let width  = windows_info.drawer3D_width in
    let height = windows_info.drawer3D_height in
    


    let distance = (* fisheye fix *)
        let ca = ((float_of_int level.player.view_angle) -. ray.angle) 
                    *. (Float.pi /. 180.0) in
        let ca = 
            if ca < 0. then ca +. 2. *. Float.pi 
            else if ca > (2. *. Float.pi) then ca -. 2. *. Float.pi 
            else ca in
        ray.distance *. cos(ca) in
    let win_step = int_of_float (float_of_int width /. ((ray.angle_max -. ray.angle_min))) in 
    let rect_height = (int_of_float ( float_of_int(height) /. distance)) in
    let rec_start_X = int_of_float ((ray.angle_max -. ray.angle) *. (float_of_int win_step)) in
    let rec_start_Y = height/2 - rect_height/2 in

    let intersect_x = 
        let decimal = Float.round(ray.intersection.x *. 100.) /. 100. in
        fst (Float.modf decimal) in
    let intersect_y = 
        let decimal = Float.round(ray.intersection.y *. 100.) /. 100. in
        fst (Float.modf decimal) in
    let in_texture_intersection = intersect_x +. intersect_y in 

    let tx = 
        let tmp = int_of_float (in_texture_intersection *. 64.) in
        if (ray.angle_vec.x < 0. && float_approx_eq intersect_x 0. 0.01 ) ||
            (ray.angle_vec.y > 0. && intersect_x <> 0.) then
            63-tmp
        else
            tmp
    in

    let wall = level.map.plot.(int_of_float ray.touched_pos.y).(int_of_float ray.touched_pos.x) in
    let texture = texture_to_plot_tile textures wall in

    Sdlrender.copyEx 
        windows_info.render 
        ~texture:texture 
        ~src_rect:(Sdlrect.make ~pos:(tx,0) ~dims:(1,64))
        ~dst_rect:(Sdlrect.make ~pos:(rec_start_X, rec_start_Y) 
                    ~dims:(win_step, rect_height))
        ();

;;

let renderHud windows_info = 
    let position_hud_x = 0 in 
    let position_hud_y = windows_info.drawer2D_height - windows_info.drawer2D_height / 5 in  
    
    let img = Sdltexture.create_from_surface 
        windows_info.render (Sdlsurface.load_bmp ~filename:("main/resources/textures/hud.bmp"))
        in 

    let dst_rect = Sdlrect.make 
        ~pos:(position_hud_x,position_hud_y)
        ~dims:((windows_info.drawer2D_width * 2),200) in

    Sdlrender.copyEx 
        windows_info.render 
        ~texture:img 
        ~src_rect:(Sdlrect.make ~pos:(0,0) ~dims:(600,100))
        ~dst_rect: dst_rect
        ~angle:0.
        ();
;;

let renderPV windows_info level =
    let position_hud_x = 0 in 
    let position_hud_y = windows_info.drawer2D_height - windows_info.drawer2D_height / 5 in  

    let pv_courant = string_of_int level.player.hp in 

    let img = Sdltexture.create_from_surface 
        windows_info.render (Sdlsurface.load_bmp ~filename:("main/resources/textures/"^pv_courant^"PV.bmp"))
        in 

    let dst_rect = Sdlrect.make 
        ~pos:(position_hud_x+200,position_hud_y)
        ~dims:(350,200) in
    

    Sdlrender.copyEx 
        windows_info.render 
        ~texture:img 
        ~src_rect:(Sdlrect.make ~pos:(0,0) ~dims:(600,100))
        ~dst_rect: dst_rect
        ~angle:0.
        ();

        

    ()
;;

let renderArme windows_info = 
    let position_hud_x = windows_info.drawer2D_width / 2 + (windows_info.drawer2D_width / 5) in 
    let position_hud_y = windows_info.drawer2D_height - windows_info.drawer2D_height / 2 + 25 in      

    let img = Sdltexture.create_from_surface 
        windows_info.render (Sdlsurface.load_bmp ~filename:("main/resources/textures/arme.bmp"))
        in 

    let dst_rect = Sdlrect.make 
        ~pos:(position_hud_x,position_hud_y)
        ~dims:((300),210) in
    

    Sdlrender.copyEx 
        windows_info.render 
        ~texture:img 
        ~src_rect:(Sdlrect.make ~pos:(0,0) ~dims:(600,100))
        ~dst_rect: dst_rect
        ~angle:0.
        ();

    
;;

let renderShotgunBlast windows_info = 
    print_string("SHOT");
    let position_hud_x = windows_info.drawer2D_width / 2 + (windows_info.drawer2D_width / 5) in 
    let position_hud_y = windows_info.drawer2D_height - windows_info.drawer2D_height / 2 + 25 in      

    let img = Sdltexture.create_from_surface 
        windows_info.render (Sdlsurface.load_bmp ~filename:("main/resources/textures/shotgun_blast.bmp"))
        in 

    let dst_rect = Sdlrect.make 
        ~pos:(position_hud_x,position_hud_y-120)
        ~dims:((300),210) in
    

    Sdlrender.copyEx 
        windows_info.render 
        ~texture:img 
        ~src_rect:(Sdlrect.make ~pos:(0,0) ~dims:(600,100))
        ~dst_rect: dst_rect
        ~angle:0.
        ();

    (*reload := true ; *)
    let time_now = Unix.localtime (Unix.time ()) in 
    sec_fin := float_of_int time_now.tm_sec +. 2.0;
;;

let renderReload windows_info = 

    let position_hud_x = windows_info.drawer2D_width / 2 + (windows_info.drawer2D_width / 5) in 
    let position_hud_y = windows_info.drawer2D_height - windows_info.drawer2D_height / 2 + 25 in      

    let img = Sdltexture.create_from_surface 
        windows_info.render (Sdlsurface.load_bmp ~filename:("main/resources/textures/reload1.bmp"))
        in 

    let dst_rect = Sdlrect.make 
        ~pos:(position_hud_x,position_hud_y)
        ~dims:((300),210) in
    

    Sdlrender.copyEx 
        windows_info.render 
        ~texture:img 
        ~src_rect:(Sdlrect.make ~pos:(0,0) ~dims:(600,100))
        ~dst_rect: dst_rect
        ~angle:0.
        ();
        
        (*reload := false; *)

    ()
;;

let renderReloadPart2 windows_info = 

    let position_hud_x = windows_info.drawer2D_width / 2 + (windows_info.drawer2D_width / 5) in 
    let position_hud_y = windows_info.drawer2D_height - windows_info.drawer2D_height / 2 + 25 in      

    let img = Sdltexture.create_from_surface 
        windows_info.render (Sdlsurface.load_bmp ~filename:("main/resources/textures/reload2.bmp"))
        in 

    let dst_rect = Sdlrect.make 
        ~pos:(position_hud_x,position_hud_y)
        ~dims:((300),210) in
    

    Sdlrender.copyEx 
        windows_info.render 
        ~texture:img 
        ~src_rect:(Sdlrect.make ~pos:(0,0) ~dims:(600,100))
        ~dst_rect: dst_rect
        ~angle:0.
        ();
        
        (*reload := false; *)

    ()
;;

let renderReloadPart3 windows_info = 
    print_string "hey \n";

    let position_hud_x = windows_info.drawer2D_width / 2 + (windows_info.drawer2D_width / 5) in 
    let position_hud_y = windows_info.drawer2D_height - windows_info.drawer2D_height / 2 + 25 in      

    let img = Sdltexture.create_from_surface 
        windows_info.render (Sdlsurface.load_bmp ~filename:("main/resources/textures/reload3.bmp"))
        in 

    let dst_rect = Sdlrect.make 
        ~pos:(position_hud_x,position_hud_y)
        ~dims:((300),210) in
    

    Sdlrender.copyEx 
        windows_info.render 
        ~texture:img 
        ~src_rect:(Sdlrect.make ~pos:(0,0) ~dims:(600,100))
        ~dst_rect: dst_rect
        ~angle:0.
        ();
        
        (*reload := false; *)

    ()
;;


(*TODO bitmap to STRING ? *)




let render windows_info level textures rays = 
    (* Rendering the sky *)
    if level.map.ceiling then (
        let text = List.assoc "sky" textures in
        Sdlrender.copyEx 
            windows_info.render 
            ~texture:text
            ~src_rect:(Sdlrect.make ~pos:(0,0) ~dims:(640-1,480-1))
            ~dst_rect:(Sdlrect.make ~pos:(0,0) 
                ~dims:(windows_info.drawer3D_width, windows_info.drawer3D_height/2))
            ();
    );

    (* Rendering the rays *)
    List.iter (fun ray -> drawRay windows_info level textures ray) rays;

    renderHud windows_info;
    renderPV windows_info level;
    (*renderArme windows_info; *)

            (* debut animation *)
            let localtime = Unix.localtime (Unix.time ()) in 
            if float_of_int (localtime.tm_sec) < !sec_fin -. 1.5 then ( (* Animation *)    
            renderReload windows_info ;
            ) else if float_of_int (localtime.tm_sec) < !sec_fin -. 0.75 then (renderReloadPart2 windows_info;) 
            else if float_of_int (localtime.tm_sec) < !sec_fin  then (renderReloadPart3 windows_info;) 
            else renderArme windows_info;
    
            (* fin animation *)



    (*if( !reload = false ) then (renderArme windows_info;) else renderReload windows_info; *)


    ();;


