open AST
open Bogue
module W = Widget
module L = Layout
module T = Trigger
module A = Sdl_area
module S = Style


let drawRay windows_info level ray = 
    
    if not (ray.rayTouched) then (failwith "ray not touched") else
    
    (* fisheye fix *)
    let ca = (float_of_int level.player.view_angle) -. (float_of_int ray.angle) in
    let ca = ca *. (Float.pi /. 180.0) in (* deg to rad *)
    let ca = if ca < 0. then ca +. 2. *. Float.pi 
            else if ca > (2. *. Float.pi) then ca -. 2. *. Float.pi 
            else ca in
    let distance = ray.distance *. cos(ca) in

    let win_step = windows_info.width / (ray.angle_max - ray.angle_min) in 
    let rect_height = (int_of_float ( float_of_int(windows_info.height) /. distance)) in

    let t = ray.angle_max - ray.angle in
    let rec_start = (
        t * win_step,
        windows_info.height/2 - rect_height/2
    ) in

    let (r,g,b) = Drawer2D.color_of_wall 
        level.plot.(int_of_float ray.touched_pos.y).(int_of_float ray.touched_pos.x) in
    let r = if r = 0 then 0 else max 0 (r - int_of_float (30. *. distance)) in
    let g = if g = 0 then 0 else max 0 (g - int_of_float (30. *. distance)) in
    let b = if b = 0 then 0 else max 0 (b - int_of_float (30. *. distance)) in
    A.fill_rectangle windows_info.area3D
        ~color:(Draw.opaque (r,g,b))
        ~w:win_step
        ~h:rect_height
        rec_start;
    ();;