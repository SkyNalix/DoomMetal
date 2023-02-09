open AST
open Bogue
module W = Widget
module L = Layout
module T = Trigger
module A = Sdl_area
module S = Style


open Images
let img = match (Jpeg.load "main/resources/wolftextures32.jpg" []) with
    | Rgb24 img -> img
    | _ -> failwith "image not Rgb24";;
let img = Rgb24.sub img 0 0 32 32;;


let texture_to_ty = function
| NOTHING -> failwith "no texture for NOTHING"
| WALL -> 0
| RED_WALL -> 1 
| TRANSPARENT_WALL ->  failwith "no texture for TRANSPARENT_WALL"

let drawRay windows_info level ray =
    if not (ray.rayTouched) then (failwith "ray not touched") else
    let distance = (* fisheye fix *)
        let ca = ((float_of_int level.player.view_angle) -. (float_of_int ray.angle)) 
                    *. (Float.pi /. 180.0) in
        let ca = 
            if ca < 0. then ca +. 2. *. Float.pi 
            else if ca > (2. *. Float.pi) then ca -. 2. *. Float.pi 
            else ca in
        ray.distance *. cos(ca) in
    let win_step = windows_info.width / ((ray.angle_max - ray.angle_min)/ray.angle_step) in 
    let rect_height = (int_of_float ( float_of_int(windows_info.height) /. distance)) in
    let rec_start_X = (ray.angle_max - ray.angle) * win_step in
    let rec_start_Y = windows_info.height/2 - rect_height/2 in

    if not windows_info.parameters.textured then (
        A.fill_rectangle 
            windows_info.area3D
            ~color:(Draw.opaque (Drawer2D.color_of_wall level.plot.
                        (int_of_float ray.touched_pos.y).(int_of_float ray.touched_pos.x)))
            ~w:(win_step)
            ~h:(rect_height)
            (rec_start_X, rec_start_Y);
    ) else

    let intersect_x = 
        let decimal = Float.floor ray.intersection.x in
        if decimal = 0. then 0. else ray.intersection.x -. decimal in
    let intersect_y = 
        let decimal = Float.floor ray.intersection.y in
        if decimal = 0. then 0. else ray.intersection.y -. decimal in
    let in_texture_intersection = intersect_y +. intersect_x in
    let lineH = rect_height in
    let ty_step = 32. /. (float_of_int lineH) in

    let tx = 
        let tmp = int_of_float (in_texture_intersection *. 32.) in
        if (ray.angle_vec.x < 0. && intersect_y <> 0.) ||
            (ray.angle_vec.y > 0. && intersect_x <> 0.) then
            31-tmp mod 32
        else
            tmp mod 32
    in

    let rec loopi i ty = 
        if i >= lineH-1 then () else
        let c = Rgb24.get img tx  (int_of_float ty) in
        A.fill_rectangle 
            windows_info.area3D
            ~color:(c.r,c.g,c.b,255)
            ~w:(win_step)
            ~h:(1)
            (rec_start_X, rec_start_Y+i);

        loopi (i+1) (ty +. ty_step)
        in 
    loopi 0 0.;

    ();;
