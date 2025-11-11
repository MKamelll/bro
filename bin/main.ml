type tokentype =
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | String of string
  | Int of int
  | Float of float
  | Let
  | Const
  | Type
  | Colon
  | SemiColon
  | Identifier
  | Function
  | Class
  | Plus
  | Star
  | Slash
  | StarEqual
  | SlashEqual
  | PlusPlus
  | PlusEqual
  | Minus
  | MinusMinus
  | MinusEqual
  | Equal
  | EqualEqual
  | GreaterThan
  | LessThan
  | GreaterThanEqual
  | LessThanEqual
  | And
  | Or
  | Return
  | Not
  | Unknown
  | Eof
[@@deriving show]

type lexemetype = String of string | Int of int | Float of float
[@@deriving show]

type token = { kind : tokentype; lexeme : lexemetype } [@@deriving show]

type lexer = { src : string; tokens : token list; index : int }
[@@deriving show]

let create_lexer s = { src = s; tokens = []; index = 0 }
let is_at_end l = l.index >= String.length l.src
let advance l = { src = l.src; tokens = l.tokens; index = l.index + 1 }
let curr l = l.src.[l.index]

let add_token token_type lexeme l =
  {
    src = l.src;
    tokens = { kind = token_type; lexeme } :: l.tokens;
    index = l.index;
  }

let get_tokens l = l.tokens |> List.rev

let create_identifier l =
  let rec aux l chars =
    if is_at_end l || not (Char.Ascii.is_alphanum (curr l)) then
      let identifier = chars |> List.rev |> List.to_seq |> String.of_seq
      in
      match identifier with
      | "let" -> l |> add_token Let (String identifier)
      | "const" -> l |> add_token Const (String identifier)
      | "function" -> l |> add_token Function (String identifier)
      | "class" -> l |> add_token Class (String identifier)
      | "return" -> l |> add_token Return (String identifier)
      | "and" -> l |> add_token And (String identifier)
      | "or" -> l |> add_token Or (String identifier)
      | "not" -> l |> add_token Not (String identifier)
      | "type" -> l |> add_token Type (String identifier)
      | _ -> l |> add_token Identifier (String identifier)
    else
      aux (l |> advance) ((curr l) :: chars)
  in
  aux l []

let create_string l =
  let rec aux l chars =
    if is_at_end l || curr l == '"' then
      let str = chars |> List.rev |> List.to_seq |> String.of_seq
      in
      l |> advance |> add_token (String str) (String str)
    else
      aux (l |> advance) ((curr l) :: chars)
  in
  aux (l |> advance) []

let create_number l =
  let rec aux l is_float chars =
    if not (is_at_end l) && curr l == '.'
    then
      aux (l |> advance) true ('.' :: chars)
    else
    if is_at_end l || not (l |> curr |> Char.Ascii.is_digit)
    then
      if is_float
      then
        let number = chars |> List.rev |> List.to_seq |> String.of_seq |> float_of_string
        in
        l |> add_token (Float number) (Float number)
      else
        let number = chars |> List.rev |> List.to_seq |> String.of_seq |> int_of_string
        in
        l |> add_token (Int number) (Int number)
    else
      aux (l |> advance) is_float ((curr l) :: chars)
  in
  aux l false []

let rec tokenize l =
  if is_at_end l then l |> add_token Eof (String "Eof")
  else
    match curr l with
    | '(' -> l |> add_token LeftParen (String "(") |> advance |> tokenize
    | ')' -> l |> add_token RightParen (String ")") |> advance |> tokenize
    | '[' -> l |> add_token LeftBrace (String "[") |> advance |> tokenize
    | ']' -> l |> add_token RightBrace (String "]") |> advance |> tokenize
    | '{' -> l |> add_token LeftBracket (String "{") |> advance |> tokenize
    | '}' -> l |> add_token RightBracket (String "}") |> advance |> tokenize
    | '+' ->
        let l = l |> advance in
        if (not (is_at_end l)) && curr l == '=' then
          l |> add_token PlusEqual (String "+=") |> advance |> tokenize
        else if (not (is_at_end l)) && curr l == '+' then
          l |> add_token PlusPlus (String "++") |> advance |> tokenize
        else l |> add_token Plus (String "+") |> tokenize
    | '-' ->
        let l = l |> advance in
        if (not (is_at_end l)) && curr l == '=' then
          l |> add_token MinusEqual (String "-=") |> advance |> tokenize
        else if (not (is_at_end l)) && curr l == '-' then
          l |> add_token MinusMinus (String "--") |> advance |> tokenize
        else l |> add_token Minus (String "-") |> tokenize
    | '*' ->
        let l = l |> advance in
        if (not (is_at_end l)) && curr l == '=' then
          l |> add_token StarEqual (String "*=") |> advance |> tokenize
        else l |> add_token Star (String "*") |> tokenize
    | '/' ->
        let l = l |> advance in
        if (not (is_at_end l)) && curr l == '=' then
          l |> add_token SlashEqual (String "=") |> advance |> tokenize
        else l |> add_token Slash (String "/") |> tokenize
    | '=' ->
        let l = l |> advance in
        if (not (is_at_end l)) && curr l = '=' then
          l |> add_token EqualEqual (String "==") |> advance |> tokenize
        else l |> add_token Equal (String "=") |> tokenize
    | '>' ->
        let l = l |> advance in
        if (not (is_at_end l)) && curr l = '=' then
          l |> add_token GreaterThanEqual (String ">=") |> advance |> tokenize
        else l |> add_token GreaterThan (String ">") |> tokenize
    | '<' ->
        let l = l |> advance in
        if (not (is_at_end l)) && curr l = '=' then
          l |> add_token LessThanEqual (String "<=") |> advance |> tokenize
        else l |> add_token LessThan (String "<") |> tokenize
    | ' ' -> l |> advance |> tokenize
    | ':' -> l |> add_token Colon (String ":") |> advance |> tokenize
    | ';' -> l |> add_token SemiColon (String ";") |> advance |> tokenize
    | char when Char.Ascii.is_letter char -> l |> create_identifier |> tokenize
    | char when Char.Ascii.is_digit char -> l |> create_number |> tokenize
    | '"' -> l |> create_string |> tokenize
    | _ -> l |> add_token Unknown (String (String.make 1 (curr l))) |> advance |> tokenize

let src = "( ) [ ] / * = + - \"fuck off\" function let const return 14234 12.45"
let tokens = create_lexer src |> tokenize |> get_tokens
let () = List.iter (fun x -> show_token x |> print_endline) tokens
