let test = 123. *. 123.

let test = ("hello" ^ " " ^ "world")^ (string_of_bool true)

let string_of_char v = String.make 1 'c';;
(* string.make creates string of number of variables 10 x = x 10 times*)
(*let test  = ("hello" ^ " " ^ "world")^(string_of_char 'c')*)

(* printing*)
print_endline "helloworld123"
let x = ()
let _ = Printf.printf "hello %d world" 123

let plus1 x = x+1

let plus x y = x+y


(* recurisve functions*)

(*factorial recursion*)

let rec fact n = 
  if n <= 0 then 1
  else n * (fact(n-1))

(* fibonacci*)
let rec fibo n=
  if n <=0 then 0 else
  if n = 1 then 1 else
    (fibo (n-1))+ (fibo (n-2))




let rec lesser (x:int)(ls:int list): int list = 
  match ls with
  | []->[]
  | h ::ls' ->
    (*print_int h*)
    if h < x then h :: lesser x ls'
    else lesser x ls'


let rec greater (x:int)(ls:int list): int list = 
  match ls with
  | []->[]
  | h ::ls' ->
    if h >= x then h :: lesser x ls'
    else greater x ls'


let rec qsort (ls:int list)= 
  match ls with
  | [] ->[]
  | h :: ls' ->
    let lt = lesser h ls' in
    let ge = greater h ls' in
    qsort lt@ [h] @ qsort ge
