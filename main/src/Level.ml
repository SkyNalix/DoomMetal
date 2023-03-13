open Ast

let get level =
  let filename = "main/resources/levels/" ^ level ^ ".lvl" in
  LevelParser.level LevelLexer.main (Lexing.from_channel (open_in filename))