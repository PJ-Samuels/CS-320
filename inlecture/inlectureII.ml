type const = Zero | One

type var = A | B | C | D

type term = Var of var| Const of const

type expr = Add of term*term | Minus of term*term

let const_to_string ( c:char): const option =
  match c with 
  |'0' -> Some Zero
  |'1' -> Some One
  | _ -> None

(* 
<expr>::= (<expr> + <expr>) | <digit>
<digit> ::= 0|1|2|3|4|5|6|7|8|9
*)

(*let rec expr() = 
  let* x = const in expr_l x
  and
  expr_l x =  
  (let* _ =char '+' in 
  let*  y = const in 
  (expr_l (Add(x,y)))) <|> pure x

*)

(* note dont use the large number constructor made in the inlecture, use this instead *)
type arith = Add of arith*arith| Mult of arith * arith| D of int

(*let rec expr_a(u:unit): arith parser = 
  (let* x = term_a u in 
  let* _ = char '+' in
  let* y= expr_a u in
  pure (Add(x,y))) <|> term_a u
  and let rec term_a(u:unit): arith parser = 
  (let* x = term_a u in 
  let* _ = char '*' in
  let* y= expr_a u in
  pure (Mult(x,y))) <|> factor_a u

  and let rec factor_a(u:unit): arith parser = 
  (let* _ = char '(' in
  let* _ = ws in
  let* x = expr_a() in
  let* _ = ws in
  let* _ = char ')' in
  pure x)
  <|> (let* v = natural in 
  pure (D V)


  last left check inlecture
  let parse_exp_r (x:string) = parse (expr_a ()) x 
*)



