(*parser combinators
  parser = function, input and output
  parser will always input start with a string
*)

let explode s = List.of_seq( String.to_seq s)
let implode cs  = String.of_seq( List.to_seq cs)

let getFirstCharacter s =
  match (explode s) with 
    [] -> None
  |
    h::t -> Some(h,implode t)

let getFirstThreeCharacters s = 
  match (getFirstCharacter s) with
    None -> None
  |
    Some (e,s1) -> match( getFirstCharacter s1) with
      None -> None
    |
      Some (e1,s2) -> match( getFirstCharacter s2) with
        None -> None
      |
        Some (e2,s3) ->Some ([e;e1;e2],s3)


type 'a parser = Parser of (string -> (('a*string) list))

let parse p s= 
  match p with
    Parser f -> f s 

let getFirstCharacter =
  Parser (
    fun s -> 
      match (explode s) with 
        [] -> []
      |
        h::t -> [(h,implode t)]
  )

let satc f = 
  Parser
    (
      fun s -> 
        match (explode s) with
          [] -> []
        |
          h::t -> if f h then [(h, implode t)] else []
    )

let (>>=) blue f = 
  Parser
    (
      fun inp ->
        match (parse blue inp) with
          []->[]
        |
          (h,rest)::_-> parse (f h) rest


    )
let return p = 
  Parser
    (
      fun inp ->
        [p,inp]
    )
let getFirstThree = 
  getFirstCharacter >>= fun e1 ->
  getFirstCharacter >>= fun e2 ->
  getFirstCharacter >>= fun e3 ->
  return [e1;e2;e3]

(* >== is the bind operator *)


