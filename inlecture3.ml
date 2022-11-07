
let convertToUpperCase l = List.fold_right (fun x y -> String.uppercase_ascii x::y) l []

let sum l = List.fold_right ( fun x y -> x+y) l 0

let summatrix l = List.fold_right (fun x y -> (sum x)+y) l 0

(*  [[1;2;3];[4;5;6]] 
    fun x y -> sum x  fun x y -> sum x+y
*)


(*fold left with operaters that are left associationed or none at all and fold right for operaters that are right associetates
*)
let rec fold_right f l a = 
  match l with 
    [] -> a
  |
    h::t -> f h (fold_right f t a )

let rec fold_left f l a  = 
  match l with 
    [] -> a 
  |
    h::t -> fold_left f (f a h) t


(*  Algebraic Data types
    type nat = Zero|one|two|three
                    values
    type nat = Zero| Succ of nat
*)
type nat = Zero | Succ of nat
let z = Zero
let o = Succ Zero
let t = Succ (Succ Zero)

let rec nat_to_int n = 
  match n with 
    Zero -> 0
  |
    Succ x -> 1 + nat_to_int x 

type list320 = Empty | Cons of (int*list320)
let l = [] 

let l1 = Empty

let o1 = [1;2;3;4]

let o1 = (1,Cons(2,Cons(3,Cons(4,Empty))))


let rec sum l =
  match l with 
    [] -> 0
  |
    h::t -> h+sum t
