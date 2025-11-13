open Lexer

type expression =
  | Int of int
  | String of string
  | Float of float
  | Identifier of string
  | Binary of expression * string * expression
[@@deriving show]

type parser = { lexer : lexer; index : int; parsed_trees : expression list }
[@@deriving show]

type associativity = Left | Right [@@deriving show]

type operator = { lexeme : string; assoc : associativity; prec : int }
[@@deriving show]

let curr_token p = List.nth p.lexer.tokens p.index

let create_parser l =
  let lexer = l |> tokenize |> get_tokens in
  { lexer; index = 0; parsed_trees = [] }

let advance p =
  { lexer = p.lexer; index = p.index + 1; parsed_trees = p.parsed_trees }

(* let curr_expr p = List.nth (List.rev p.parsed_trees) p.index *)
(* let prev_expr p = List.nth (List.rev p.parsed_trees) (p.index-1) *)

let is_at_end p =
  if p.index >= List.length p.lexer.tokens then true
  else match curr_token p with { kind = Eof; _ } -> true | _ -> false

let curr_operator p =
  match (curr_token p).lexeme with String op -> Some op | _ -> None

let curr_operator_info p =
  match curr_operator p with
  | Some "=" -> Some { lexeme = "="; assoc = Right; prec = 1 }
  | Some "+=" -> Some { lexeme = "+="; assoc = Left; prec = 1 }
  | Some "==" -> Some { lexeme = "=="; assoc = Left; prec = 2 }
  | Some "!=" -> Some { lexeme = "!="; assoc = Left; prec = 2 }
  | Some ">" -> Some { lexeme = ">"; assoc = Left; prec = 3 }
  | Some ">=" -> Some { lexeme = ">="; assoc = Left; prec = 3 }
  | Some "<" -> Some { lexeme = "<"; assoc = Left; prec = 3 }
  | Some "<=" -> Some { lexeme = "<="; assoc = Left; prec = 3 }
  | Some "+" -> Some { lexeme = "+"; assoc = Left; prec = 4 }
  | Some "-" -> Some { lexeme = "-"; assoc = Left; prec = 4 }
  | Some "*" -> Some { lexeme = "*"; assoc = Left; prec = 5 }
  | Some "/" -> Some { lexeme = "/"; assoc = Left; prec = 5 }
  | Some "%" -> Some { lexeme = "%"; assoc = Left; prec = 5 }
  | Some "^" -> Some { lexeme = "^"; assoc = Right; prec = 6 }
  | _ -> None


let parse_primary p =
  match curr_token p with
  | {kind = Int i; _} -> (Int i, advance p)
  | {kind = Float f; _} -> (Float f, advance p)
  | {kind = String s; _} -> (String s, advance p)
  | {kind = Identifier; lexeme = String s} -> (Identifier s, advance p)
  | _ -> failwith "not supported"


let rec parse_expr min_prec p =
  let (lhs, p) = parse_primary p
  in
  let rec aux lhs p =
    if is_at_end p then (lhs, p)
    else
      match curr_operator_info p with
      | Some {lexeme; assoc; prec} ->
        if prec < min_prec then (lhs, p)
        else
          let next_prec =
            match assoc with
            | Left -> prec + 1
            | Right -> prec
          in
          let p = p |> advance
          in
          let (rhs, p) = p |> parse_expr next_prec
          in
          let lhs = Binary(lhs, lexeme, rhs)
          in
          aux lhs p
      | None -> (lhs, p)
  in
  aux lhs p

let rec parse p =
  if is_at_end p then p
  else
    let (expr, p) = p |> parse_expr 0
    in
    {p with parsed_trees = expr :: p.parsed_trees} |> parse

let get_parsed p = List.rev p.parsed_trees

let src = "3 * 5 + 6 ^ 12 * 5 - 2"
let lexer = create_lexer src
let () = create_parser lexer
  |> parse
  |> get_parsed
  |> List.iter (fun x -> show_expression x |> print_endline)
