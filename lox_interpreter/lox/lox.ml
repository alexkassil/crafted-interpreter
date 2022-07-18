(* open Base *)
(* open Stdio *)
open Core

(* TODO: Make this return a Result type*)
let eval source:string = source

let run_file filename =
  let file = In_channel.create filename in
  let input_lines = In_channel.input_lines file in
  let input = String.concat ~sep:"\n" input_lines in
  print_endline input;
  In_channel.close file;
  let _ = eval input in
  ()

let report = printf "[line %d] Error%s: %s"

let lox_error line =
  report line ""

let rec repl () =
  printf "> %!";
  (* read eval print loop *)
  repl @@ print_endline @@ eval In_channel.(input_line_exn stdin)

let () =
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
