(* util functions *)

let is_lower_case c =
  'a' <= c && c <= 'z'

let is_upper_case c =
  'A' <= c && c <= 'Z'

let is_alpha c =
  is_lower_case c || is_upper_case c

let is_digit c =
  '0' <= c && c <= '9'

let is_alphanum c =
  is_lower_case c ||
  is_upper_case c ||
  is_digit c

let is_blank c =
  String.contains " \012\n\r\t" c

let explode s =
  List.of_seq (String.to_seq s)

let implode ls =
  String.of_seq (List.to_seq ls)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ (loop ())
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

(* end of util functions *)

(* parser combinators *)

type 'a parser = char list -> ('a * char list) option

let parse (p : 'a parser) (s : string) : ('a * char list) option =
  p (explode s)

let pure (x : 'a) : 'a parser =
  fun ls -> Some (x, ls)

let fail : 'a parser = fun ls -> None

let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> q a ls
  | None -> None

let (>>=) = bind
let (let*) = bind

let read : char parser =
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls)
  | _ -> None

let satisfy (f : char -> bool) : char parser =
  fun ls ->
  match ls with
  | x :: ls ->
    if f x then Some (x, ls)
    else None
  | _ -> None

let char (c : char) : char parser =
  satisfy (fun x -> x = c)

let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None

let (>>) = seq

let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) ->
    (match p2 ls with
     | Some (_, ls) -> Some (x, ls)
     | None -> None)
  | None -> None

let (<<) = seq'

let disj (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls)  -> Some (x, ls)
  | None -> p2 ls

let (<|>) = disj

let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> Some (f a, ls)
  | None -> None

let (>|=) = map

let (>|) = fun p c -> map p (fun _ -> c)

let rec many (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

let rec many1 (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None

let rec many' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

let rec many1' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None

let whitespace : unit parser =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c
    then Some ((), ls)
    else None
  | _ -> None

let ws : unit parser =
  (many whitespace) >| ()

let ws1 : unit parser =
  (many1 whitespace) >| ()

let digit : char parser =
  satisfy is_digit

let natural : int parser =
  fun ls ->
  match many1 digit ls with
  | Some (xs, ls) ->
    Some (int_of_string (implode xs), ls)
  | _ -> None

let literal (s : string) : unit parser =
  fun ls ->
  let cs = explode s in
  let rec loop cs ls =
    match cs, ls with
    | [], _ -> Some ((), ls)
    | c :: cs, x :: xs ->
      if x = c
      then loop cs xs
      else None
    | _ -> None
  in loop cs ls

let keyword (s : string) : unit parser =
  (literal s) >> ws >| ()

(* end of parser combinators *)


(* <expr> ::=  <expr> + <const>  | <const>
   <const> ::= 0|1|2|3|4|5|6|7|8|9
*)

type digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine

type expr = Add of expr * expr | D of digit

let const =  let* x = digit in (match x with
    '0' -> pure (D Zero)
    |'1' -> pure (D One) 
    |'2' -> pure (D Two)
    |'3' -> pure (D Three)
    |'4' -> pure (D Four)
    |'5' -> pure (D Five)
    |'6' -> pure (D Six)
    |'7' -> pure (D Seven)
    |'8' -> pure (D Eight)
    |'9' -> pure (D Nine)
    |_ ->  fail)

let rec expr() =
  (let* x=expr() in
   let* _= char '+' in
   let* y=const in pure (Add (x,y))) <|> const









let rec expr() =
  let rec expr_l x =
    (let* _= char '+' in
     let* y= const in
     (expr_l (Add(x,y)))) <|>  pure x
  in
  let* x=const in expr_l x 




(*
Grammar:
<expr> := <term> + <expr> | <term>
<term> = <factor> * <term> | <factor> 
<factor> = ( expr ) | natural
 *)

type arith = Add of arith * arith | Mult of arith * arith | D of int

let rec expr_a (u:unit) : arith parser  = 
  (let* x = term_a u in 
   let* _= char '+' in 
   let* y = expr_a u in 
   pure (Add(x,y))) <|> term_a u 

and term_a (u:unit) : arith parser  = 
  (let* x = factor_a u in 
   let* _ = char '*' in 
   let* y = term_a u in 
   pure (Mult(x,y))) <|> factor_a u 

and factor_a (u:unit) : arith parser=
  (let* _= char '(' in
   let* _= ws in
   let* x= expr_a() in
   let* _= ws in
   let* _= char ')' in 
   pure x )
  <|>  (let* v = natural in 
        pure (D v))

let parse_exp_r (x:string) = parse (expr_a ()) x 

(*
Grammar:
<A> ::= a<B>
<A> ::= e
<B> ::= b<C>
<C> ::= c<A>
 *)

type gram = E | A of gram | B of gram | C of gram

let rec expr_a (u:unit) : gram parser  = 
  (let* _= char 'a' in 
   let* y = expr_b u in 
   pure (A y)) <|> (let* _= char 'e' in pure E)

and expr_b (u:unit) : gram parser  = 
  (let* _= char 'b' in 
   let* y = expr_c u in 
   pure (B y))

and expr_c (u:unit) : gram parser=
  (let* _= char 'c' in 
   let* y = expr_a u in 
   pure (C y))

let parse_expr_a (x:string) = parse (expr_a ()) x


(* if we think using regular expresssions *)

(* (abc)*e *)                        

let recognize_expr (x:string) =
  parse (let* _ = many (keyword "abc") in keyword "e" ) x

(* (a*b*c)*e *)                        

let recognize_expr_1 (x:string) =
  parse (let* _ = many (let* _ = many (keyword "a") in
                        let* _ = many (keyword "b") in
                        (keyword "c"))  in keyword "e" ) x
