open Ast

let get level =
  let filename = "main/resources/levels/" ^ level ^ ".lvl" in
  LevelParser.level LevelLexer.main (Lexing.from_channel (open_in filename))

let touchedBlockAction game x y = 
  match game.level.map.plot.(y).(x) with
    | DOOR -> game.level.map.plot.(y).(x) <- NOTHING
    | LEVEL_END -> 
      game.state <- LEVEL_FINISHED;
      game.level <- get "1";
    | _ -> ()
  ;;