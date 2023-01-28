open AST
open Bogue
module W = Widget
module L = Layout
module T = Trigger
module A = Sdl_area
module S = Style

open Images
let img = match (Png.load_as_rgb24 "main/resources/wolftextures.png" []) with
    | Rgb24 img -> img
    | _ -> failwith "impossible";;

(* let img = Rgb24.sub img 0 0 64 64;; *)
let img = Rgb24.sub img 64 0 64 64 ;;

let drawRay windows_info level ray =
    
    if not (ray.rayTouched) then (failwith "ray not touched") else

    (* fisheye fix *)
    let ca = (float_of_int level.player.view_angle) -. (float_of_int ray.angle) in
    let ca = ca *. (Float.pi /. 180.0) in (* deg to rad *)
    let ca = if ca < 0. then ca +. 2. *. Float.pi 
            else if ca > (2. *. Float.pi) then ca -. 2. *. Float.pi 
            else ca in
    let distance = ray.distance *. cos(ca) in
    (* let distance = ray.distance in *)

    let win_step = windows_info.width / (ray.angle_max - ray.angle_min) in 
    let rect_height = (int_of_float ( float_of_int(windows_info.height) /. distance)) in

    let t = ray.angle_max - ray.angle in
    let rec_start_X = t * win_step in
    let rec_start_Y = windows_info.height/2 - rect_height/2 in

    (*drawing the pixels*)

    let intersect_x = ray.intersection.x -. (Float.floor ray.intersection.x) in
    let intersect_y = ray.intersection.y -. (Float.floor ray.intersection.y) in
    let test = if intersect_x <> 0. then intersect_x else intersect_y in 
    let test = test -. Float.floor test in
    let test =  (int_of_float (test *. (float_of_int windows_info.block_width))) in
    let img = Rgb24.sub img 
            (0+test)
            0
            (64/win_step)
            64 in
            
    let pixel_size_y = rect_height/64 in 
    for i = 0 to img.width-1 do
        for j = 0 to img.height-1 do
            let rgb = Rgb24.get img i j in

            A.fill_rectangle 
                windows_info.area3D
                ~color:(rgb.r,rgb.g,rgb.b,255)
                ~w:(win_step)
                ~h:(rect_height/64)
                (i + rec_start_X, pixel_size_y * j + rec_start_Y)
        done; 
    done;
    ();;