(*regular expressions*)

(*  a*bc*  

    remember BNF uses ::= and is recursive

    regular is just ex.) a+bc*

    (push const; p/S) -> (p/const::S)
*)


(*type const = 
  |Nat of int
  |Name of string
  |Unit*)

type const = int

type command = 
  |Push of const
  |Pop
  |Add
  |Sub
and commands = command list

let parse_command = 
  (keyword "push" >> let* c= natural in pure (Push c))<|>
  (keyword "pop" >> pure Pop) <|>
  (keyword "Add" >> pure Add) <|>
  (keyword "Sub" >> pure Sub)

let parse_commands = 
  parse_command >>= fun fst ->
  (many (keyword ";" >> parse_command))>>= fun rest ->
  pure (fst::rest)
let rec eval p st : const list = 
  match p with 
  |Push c:: p -> eval p (c::st)
  |Pop:: p, _::st -> eval p st
  |Pop:: p, [] -> None
  |Add:: p,v2::v1::st -> eval p (v1+v2::st)
  |Add:: p,_ -> None
  |Sub:: p,v2::v1::st -> eval p (v1-v2::st)
  |Sub:: p,_ -> None
  | [],st -> Some st
let interpreter (src: string):const list option 
    match parse parse_commands src with
    |Some (p,[]) -> eval p []
    |_ -> None
