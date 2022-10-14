open Core
open Parser
let had_error = ref false

let report = printf "[line %d] Error%s: %s\n"

let lox_error line =
  had_error := true;
  report line ""

let lox_error_token (token : Token.t) =
  had_error := true;
  if Token.equal_token_type token.token_type Token.EOF then
    report token.line " at end"
  else
    report token.line (" at '" ^ token.lexeme ^ "'")


(* TODO: Make this return a Result type *)
let eval source =
  let tokens = Scanner.scan_tokens source lox_error in
  let str = List.fold tokens ~init:"" ~f:(fun prev token -> prev ^ "\n" ^ Token.show token) in
  print_endline str;
  let statements = Parser.parse {tokens = Array.of_list tokens; current = 0; error = lox_error_token} in
  print_endline (Parser.show_statements statements);
  print_endline (Parser.show_statements_pp statements);
  List.iter statements ~f:Eval.eval_statement
  (* (Eval.eval_statement statement) *)

let run_file filename =
  let file = In_channel.create filename in
  let input_lines = In_channel.input_lines file in
  let input = String.concat ~sep:"\n" input_lines in
  print_endline input;
  In_channel.close file;
  eval input;
  if !had_error then
    exit 65


let rec repl () =
  had_error := false;
  printf "> %!";
  (* read eval loop *)
  repl @@ eval In_channel.(input_line_exn stdin)

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
