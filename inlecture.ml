
let x = 5
(*let outerradius = 5.0 in 
  let innerradius = 3.0 in
  let outerarea = 3.14*.(outerradius*.outerradius) in 
  let innerarea = 3.14*.(innerradius*.innerradius) in
  let areaofring = outerarea-.innerarea in
  areaofring
*)
let x = fun y->y+1
(*normal recusion *)
let rec factorial = fun x->
  if x=1 then 1 else factorial (x-1)*x
let rec factorial x = 
  if x=1 then 1 else factorial (x-1)*x


(*tail recusion*)
let factorialT = fun x->
  let rec aux = fun accum r ->
    if r = 1 then accum
    else aux (accum*r)(r-1) 
  in
  aux 1 x
let _ =  
  print_int( factorialT 5)
(*let main = 
  print_newline();
  print_int (factorialT 5);
  print_newline()
*)


(* Lists*)
let l = []

let l = 5::[]

let l = 4::3::2::1::[]
(*l = [1;2;3]      in terminal*) 
(* 1::2::3::[]*)

let rec length = fun l ->
  match l with 
    [] -> 0
  |
    _::t -> 1+length t

let lengthT = fun l ->
  let rec aux = fun accum l->
    match l with 
      [] ->accum
    |
      h::t -> aux(accum+1) t
  in aux 0 l

let rec print_list l = 
  match l with
    [] -> ()
  |
    h::t -> print_int h;print_string ";"; print_list t 


let rec reverse l = 
  match l with
    [] -> []
  |
    h::t -> (reverse t) @ [h] 

let reverseT l = 
  let rec aux = fun accum l ->(*expression1*)
    match l with (*expression2*)
      [] -> accum
    |
      h ::t ->aux (h :: accum) t 
  in aux [] l

let _= print_list (reverseT [1;2;3;4])

let _ = 
  reverse l= [1;2;3;4]

let _ =  
  print_newline();
  print_int (lengthT [(fun x ->x+1);(fun y->y-1)]);
  print_newline()

(* ^ concatinates strings and @ concatinates lists of numbers*)



(** list <t> = empty| cons(T, List<T>)      are all linked lists
    [1,2,3] = const(1,cons2,cons3,cons3, empty)
    :: = []
    | T::List<T>
    = 

*)

(* f:(A->B) .... x*)

(* Sept 21 2021

   value        type
   5            int
   some 5        int option
   some 3.14     float option
   none          'a option



   int list -> sum -> int option
   [] -> sum -> none
   [1] -> sum -> some 1
   [1,2,3] -> sum -> some 6
*)

let rec sum l = 
  match l with
    [] -> None
  |
    [h]-> Some h
  |
    h::t -> match( sum t) with 
      None -> None(**this line will never get hit *)
    |
      Some v -> Some(v+h)

let print_option m=
  match m with
    None -> ()
  |
    Some value -> print_int value
let _=
  print_option(sum[1;2;3;4;5;6])

let slopenew p1 p2 = 
  let (x1,y1),(x2,y2) = (p1,p2)
  in if x2-.x1 = 0.0 then None else Some ((y2-.y1)/.(x2-.x1))


let slope p1 p2 = 
  match p1,p2 with
    ((x1,y1),(x2,y2)) -> if x2-.x1 = 0.0 then None else Some ((y2-.y1)/.(x2-.x1))




