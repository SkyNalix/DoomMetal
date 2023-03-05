open AST

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
  ) else level
;;

let update_player level x y view_angle =
  let a = int_of_float (level.player.pos.x +. x ) in 
  let b = int_of_float (level.player.pos.y +. y  ) in
  if level.plot.(b).(a) = NOTHING then (
      level.player <- { level.player with
          pos = {
              x=(level.player.pos.x +. x);
              y=(level.player.pos.y +. y)
          };
          view_angle = (level.player.view_angle + view_angle) mod 360;
      }
  );
  level;;

