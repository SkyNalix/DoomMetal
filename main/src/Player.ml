open Ast

let rec shoot y_change x_change level (player:player) enemies = 
  let px = player.pos.x in
  let py = player.pos.y in
  let rec aux liste  =
      match liste with
      | [] -> ()
      | enn  :: l ->  
          if Common.arrondir enn.pos.y py 0.35 && (Common.arrondir enn.pos.x px 0.35) then (
              enn.hp <- enn.hp -1; 
              level.enemies <- List.filter (fun enn -> enn.hp > 0) level.enemies
          ) else aux l
  in

  if py < 8. && py > 0. && px > 0. && px < 8. && (Common.toucheUnMur px py level) = false then (
      aux enemies;
      let player = {
          player with 
          pos = {
              x=(px +. x_change);
              y=(py +. y_change);
          }
      } in
      shoot y_change x_change level player enemies 
  );
;;

let update_pos level =
    let player = level.player in
    let angle_rad = (float_of_int player.view_angle) *. Float.pi /. 180.0 in
    let dx = sin angle_rad *. player.forward_speed -. 
            cos angle_rad *. player.sideway_speed in
    let dy = cos angle_rad *. player.forward_speed +. 
            sin angle_rad *. player.sideway_speed in
    let new_x = player.pos.x +. dx in
    let new_y = player.pos.y +. dy in
    if level.map.plot.(int_of_float new_y).(int_of_float new_x) = NOTHING then (
        player.pos.x <- new_x;
        player.pos.y <-new_y;   
    );
    ();;

