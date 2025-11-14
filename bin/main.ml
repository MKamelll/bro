open Lexer

type expression =
  | Int of int
  | String of string
  | Float of float
  | Identifier of string
  | Let of expression * expression
  | Const of expression * expression
  | Block of expression list
  | Binary of expression * string * expression
[@@deriving show]

type parser = { lexer : lexer; index : int; parsed_trees : expression list }
[@@deriving show]

type associativity = Left | Right [@@deriving show]

type operator = { lexeme : string; assoc : associativity; prec : int }
[@@deriving show]

let curr_token p = List.nth p.lexer.tokens p.index
let curr_token_lexeme p = show_lexemetype ((curr_token p).lexeme)

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


let rec parse_primary p =
  match curr_token p with
  | {kind = Int i; _} -> (Int i, advance p)
  | {kind = Float f; _} -> (Float f, advance p)
  | {kind = String s; _} -> (String s, advance p)
  | {kind = Identifier; lexeme = String s} -> (Identifier s, advance p)
  | {kind = Let; _} -> parse_let_expr p
  | {kind = Const; _} -> parse_const_expr p
  | {kind = LeftBracket; _} -> parse_block_expr p
  | _ -> failwith "not supported"

and expect_semicolon p =
  match curr_token p with
  | {kind = SemiColon; _} -> p |> advance
  | _ -> failwith "Expected a semicolon"

and parse_let_expr p =
  let p = p |> advance
  in
  match curr_token p with
  | {kind = Identifier; lexeme = String s} ->
    let p = p |> advance |> advance
    in
    (* show_token (curr_token p) |> print_endline; *)
    let (rhs, p) = parse_expr 0 p
    in
    let p = expect_semicolon p
    in
    (Let (Identifier s, rhs), p)
  | _ -> failwith ("Expected an identifier instead got '" ^ curr_token_lexeme p  ^ "'")

and parse_const_expr p =
  let p = p |> advance
  in
  match curr_token p with
  | {kind = Identifier; lexeme = String s} ->
    let p = p |> advance |> advance
    in
    let (rhs, p) = parse_expr 0 p
    in
    let p = expect_semicolon p
    in
    (Const (Identifier s, rhs), p)
  | _ -> failwith ("Expected an identifier instead got '" ^ curr_token_lexeme p ^ "'")

and parse_block_expr p =
  let p = p |> advance
  in
  let rec aux exprs p =
    if is_at_end p then (Block (List.rev exprs), p)
    else
      match curr_token p with
      | {kind = RightBracket; _} -> (Block (List.rev exprs), advance p)
      | _ ->
        let (expr, p) = parse_expr 0 p
        in
        aux (expr :: exprs) p
  in
  aux [] p

and parse_expr min_prec p =
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

let src = "{ let x = 3 * 5 + 6 ^ 12 * 5 - 2; const s = 15; }"
let lexer = create_lexer src
let () = create_parser lexer
  |> parse
  |> get_parsed
  |> List.iter (fun x -> show_expression x |> print_endline)
