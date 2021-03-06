open Core
open Ast
open Parser
let had_error = ref false

let report = printf "[line %d] Error%s: %s\n"

let lox_error line =
  had_error := true;
  report line ""

(* TODO: Make this return a Result type*)
let eval source:string =
  let tokens = Scanner.scan_tokens source lox_error in
  (* let _ast = Ast.parse tokens in *)
  let str = List.fold tokens ~init:"" ~f:(fun prev token -> prev ^ "\n" ^ Token.show token) in
  print_endline str;
  let expr = Parser.expression {tokens = Array.of_list tokens; current = 0} in
  print_endline (Parser.show_expression expr);
  Eval.show_lox_value (Eval.eval_expression expr)

let run_file filename =
  let file = In_channel.create filename in
  let input_lines = In_channel.input_lines file in
  let input = String.concat ~sep:"\n" input_lines in
  print_endline input;
  In_channel.close file;
  let output = eval input in
  print_endline output;
  if !had_error then
    exit 65


let rec repl () =
  had_error := false;
  printf "> %!";
  (* read eval print loop *)
  repl @@ print_endline @@ eval In_channel.(input_line_exn stdin)

let () =
  print_endline (Ast.show_expression test);
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
