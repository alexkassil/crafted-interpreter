(* open Base *)
(* open Stdio *)
open Core

let had_error = ref false

let report = printf "[line %d] Error%s: %s\n"

let lox_error line =
  had_error := true;
  report line ""

(* TODO: Make this return a Result type*)
let eval source:string =
  let tokens = Scanner.scan_tokens source lox_error in
  List.fold tokens ~init:"" ~f:(fun prev token -> prev ^ "\n" ^ Token.show token)

let run_file filename =
  let file = In_channel.create filename in
  let input_lines = In_channel.input_lines file in
  let input = String.concat ~sep:"\n" input_lines in
  print_endline input;
  In_channel.close file;
  let _ = eval input in
  if !had_error then
    exit 65


let rec repl () =
  had_error := false;
  printf "> %!";
  (* read eval print loop *)
  repl @@ print_endline @@ eval In_channel.(input_line_exn stdin)

let () =
  let l = {Token.tokenType = Token.LEFT_BRACE; lexeme = ""; literal = Some (Token.STRING_LITERAL ""); line = 0;} in
  let _ = print_endline (Token.my_show l) in
  let _ = print_endline (Token.show l) in
  let args = Sys.get_argv () in
  let args_length = Array.length args in
  if (args_length = 2) then
    run_file (Array.nget args 1)
  else if (args_length = 1) then
    repl ()
  else begin
    print_endline "Usage: lox [script]";
    exit 64
  end
