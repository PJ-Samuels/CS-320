(* map : ('a -> 'b) -> 'a list - > 'b list
   map(fun x -> x+2) [10;20;30]


   map ('a -> 'b)

   map (fun f -> 1::f) [[1;2];[3;4]]

   int list -> {quicksort}  -> int list


    list.filter(h::t h<p)
    list.filter(h::t h>p)
*)
let rec greater p l = 
  let rec aux acc l = 
    match l with 
      [] -> acc
    |
      h::t-> if h>p then aux (h::acc )t else aux acc t
  in aux [] l
let rec smaller p l = 
  let rec aux acc l = 
    match l with 
      [] -> acc
    |
      h::t-> if h<p then aux (h::acc )t else aux acc t
  in aux [] l

let rec quicksort l = 
  match l with
    [] -> []
  |
    [h] -> [h]
  |
    h::t -> let smaller = List.filter(fun x-> x<h)t
    in let greater = List.filter(fun x-> x>h)t
    in (quicksort smaller)@[h]@(quicksort greater)

(*(quicksort (smaller h t))@[h]@(quicksort (greater h t)) *)


let rec map f l =
  match l with
    [] -> []
  |
    h::t-> f h  ::  map f t 

(*let rec filter f l = 
  match l with
    [] -> []
  |
    h::t *)

let ($$) f g = fun x-> f(g x )


(*let value = tan(cos 45.0)
  let value = (tan $$ cos) 45.0
  or let value = (fun x-> tan (cosx)) 45.0

  let negate x = -1*x
  List.map (fun x -> negate(abs x) )
  or List.map (negate $$ abs)x parentheses make it possible to not use infix notation


*)
let rec sum l =
  match l with 
    [] -> 0
  |
    h::t -> h + sum t
let tail l = 
  match l with
    []->[]
  |
    h::t -> t

(*List.map (fun x -> sum (tail x)) [[1;2];[3;4;5]] ----- terminal input
  list.map (sum $$ tail)[[1;2];[3;4;5]]
*)
let rec mergesort ls = 
  match ls with
    []-> []
  |
    [x] -> ls
  |
    _-> let split ls = 
          let rec aux ls left right =
            match ls with 
              []-> (left,right)
            |
              head::tail -> aux tail right (head::left)
          in aux ls [] []
    in 
    let (left,right) = split ls
    in 
    let rec merge left right = 
      match (left, right) with
        ([],[]) -> [] 
      |
        (_,[])-> left
      |
        ([],_) -> right
      |
        (head1::tail1,head2::tail2) -> if head1> head2 then head2::(merge left tail2) else head1::(merge tail1 right)
    in merge (mergesort left) (mergesort right)


let _ = 
  mergesort [-1;-2;1;2;9;-5]


(* let reverse l = List.fold_right(fun x y -> y @[x] 1 []) *)