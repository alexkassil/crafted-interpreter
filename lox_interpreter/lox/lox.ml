(* open Base *)
(* open Stdio *)
open Core

let eval _ = "hi "

let run_file filename =
  let file = In_channel.create filename in
  let input_lines = In_channel.input_lines file in
  List.iter input_lines ~f:print_endline;
  In_channel.close file;
  let _ = eval input_lines in
  ()


let rec repl () =
  printf "> %!";
  (* read eval print loop *)
  repl @@ print_endline @@ eval [In_channel.(input_line_exn stdin)]

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
