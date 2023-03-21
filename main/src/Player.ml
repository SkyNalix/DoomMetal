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

    let time_step = 0.1 in
    let p_x = int_of_float player.pos.x in
    let p_y = int_of_float player.pos.y in
    let floor_tile = level.map.floor.(p_y).(p_x) in
    let friction_factor = Ast.friction_of_floor_tile floor_tile in

    (* apply friction *)
    player.acceleration.x <- player.acceleration.x -. friction_factor *. (player.velocity.x);
    player.acceleration.y <- player.acceleration.y -. friction_factor *. (player.velocity.y);

    let friction = friction_factor *. (sqrt (player.velocity.x ** 2.0 +. player.velocity.y ** 2.0)) in
    player.velocity.x <- player.velocity.x +. (player.acceleration.x -. friction) *. time_step;
    player.velocity.y <- player.velocity.y +. (player.acceleration.y -. friction) *. time_step;
    player.pos.x <- player.pos.x +. player.velocity.x *. time_step +. 0.5 *. player.acceleration.x *. time_step ** 2.0;
    player.pos.y <- player.pos.y +. player.velocity.y *. time_step +. 0.5 *. player.acceleration.y *. time_step ** 2.0;
    ();;

