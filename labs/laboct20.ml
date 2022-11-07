let explode s = List.of_seq(String.to_seq s)

let implode ls = String.of_seq(List.to_seq ls)


type expr = 
  |Num of int
  |Add of expr*expr
  |Mul of expr*expr

type 'a p_result = ('a*(char list)) option

let rec eval (e:expr):int = 
  match e with 
  |Num x -> x 
  |Add (e1,e2) -> (eval e1) + (eval e2)
  |Mul (e1, e2) -> (eval e1)* (eval e2)

let rec trimWS (ls: char list): char list = 
  match ls with
  | ' ':: rest -> trimWS rest
  | _ -> ls
let rec parseDigits (ls: char list): (char list) p_result = 
  match ls with 
  |[] -> None
  |h::t -> 
    if '0' <= h && h <= '9' then 
      match parseDigits t with
      |None -> Some(h :: [], trimWS t)
      |Some (digits, rest) -> Some (h::digits, trimWS rest)
    else None 
let parseNumber (ls:char list): int p_result = 
  match parseDigits ls with
  |None -> None
  |Some (digits, rest) ->
    Some(int_of_string (implode digits), trimWS rest)

let _ = match parseNumber (explode("282734jksfheojiowjrew5jgkds7")) with
  |None -> print_endline "oops"
  |Some (x,rest) -> print_endline(string_of_int x); print_endline (implode rest)

let parseAdd (ls: char list) : (expr -> expr->expr) p_result = 
  match ls with 
  | '+'::rest -> Some (((fun e1 e2 -> Add(e1,e2)), trimWS rest))
  |_ -> None

let parseMul (ls: char list) : (expr -> expr->expr) p_result = 
  match ls with 
  | '*'::rest -> Some (((fun e1 e2 -> Mul(e1,e2)), trimWS rest))
  |_ -> None
let parseExpr0 (ls : char list): expr p_result = 
  match parseNumber ls with
  |None -> None
  |Some (n,rest) -> Some(Num n, trimWS rest)
let parseExpr1 (ls : char list): expr p_result = 
  let rec loop ls lhs = 
    match parseMul ls with
    |None ->  Some(lhs,trimWS ls )
    |Some (op, rest) -> 
      match parseExpr0 rest with
      |None -> None
      |Some (rhs, rest) -> loop (trimWS rest )(op lhs rhs)
  in match parseExpr0 ls with
  |None -> None
  |Some (lhs,rest) -> loop rest lhs


let parseExpr2 (ls : char list): expr p_result = 
  let rec loop ls lhs = 
    match parseAdd ls with
    |None ->  Some(lhs,trimWS ls )
    |Some (op, rest) -> 
      match parseExpr1 rest with
      |None -> None
      |Some (rhs, rest) -> loop (trimWS rest )(op lhs rhs)
  in match parseExpr1 ls with
  |None -> None
  |Some (lhs,rest) -> loop rest lhs


let parseExpr (ls : char list): expr p_result = 
  parseExpr2 (trimWS ls)

let _ = match parseExpr1 (explode "2*2*2*2") with
  |None -> print_endline "oops"
  |Some (x,rest) -> print_endline(string_of_int(eval x)); print_endline (implode rest)

type 'a parser = char list -> ('a * char list) option