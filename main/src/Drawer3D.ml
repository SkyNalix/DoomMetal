open AST

let textures_dir = "main/resources/textures/"
let images = 
    [
    ("white_bricks", Sdlsurface.load_bmp ~filename:(textures_dir^"white_bricks.bmp"));
    ("red_bricks", Sdlsurface.load_bmp ~filename:(textures_dir^"red_bricks.bmp"));
    (* ("cobblestone", Sdlsurface.load_bmp ~filename:(textures_dir^"cobblestone.bmp")); *)
    (* ("door", Sdlsurface.load_bmp ~filename:(textures_dir^"door.bmp")); *)
    (* ("wooden_planks", Sdlsurface.load_bmp ~filename:(textures_dir^"wooden_planks.bmp")); *)
    (* ("purple_thing", Sdlsurface.load_bmp ~filename:(textures_dir^"purple_thing.bmp")); *)
    ]

let texture_to_ty = function
| NOTHING -> failwith "no texture for NOTHING"
| WALL -> 0
| RED_WALL -> 1 
| TRANSPARENT_WALL ->  failwith "no texture for TRANSPARENT_WALL"


let float_approx_eq f1 f2 approx = 
    f1 < (f2+.approx) && f1 > (f2-.approx)

let drawRay windows_info level ray =
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
        let decimal = Float.floor ray.intersection.x in
        if decimal = 0. then 0. else ray.intersection.x -. decimal in
    let intersect_y = 
        let decimal = Float.floor ray.intersection.y in
        if decimal = 0. then 0. else ray.intersection.y -. decimal in
    let in_texture_intersection = mod_float (intersect_x +. intersect_y) 1.0 in 

    let tx = 
        let tmp = int_of_float (in_texture_intersection *. 64.) in
        if (ray.angle_vec.x < 0. && float_approx_eq intersect_x 0. 0.01 ) ||
            (ray.angle_vec.y > 0. && intersect_x <> 0.) then
            63-tmp
        else
            tmp
    in

    let wall = level.plot.(int_of_float ray.touched_pos.y).(int_of_float ray.touched_pos.x) in
    let texture = Sdltexture.create_from_surface 
            windows_info.render 
            (List.assoc (Common.texture_of_wall wall) images) in

    Sdlrender.copyEx 
        windows_info.render 
        ~texture:texture 
        ~src_rect:(Sdlrect.make ~pos:(tx,0) ~dims:(1,64))
        ~dst_rect:(Sdlrect.make ~pos:(rec_start_X, rec_start_Y) 
                    ~dims:(win_step, rect_height))
        ();
    ();;


let render windows_info level rays = 
    (* Rendering the sky *)
    Sdlrender.copyEx 
        windows_info.render 
        ~texture:(
            Sdltexture.create_from_surface windows_info.render 
            (Sdlsurface.load_bmp ~filename:("main/resources/textures/sky.bmp"))
        )
        ~src_rect:(Sdlrect.make ~pos:(0,0) ~dims:(640-1,480-1))
        ~dst_rect:(Sdlrect.make ~pos:(0,0) 
            ~dims:(windows_info.drawer3D_width, windows_info.drawer3D_height/2))
        ();
    
    (* Rendering the rays *)
    List.iter (fun ray -> drawRay windows_info level ray) rays;
    ();;