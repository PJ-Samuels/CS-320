let rec factorial (n:int): int = 
  if n <= 1 then 1
  else n*(factorial (n-1))

let rec factorial1 :int -> int = fun n ->
  if n <= 1 then 1
  else n*(factorial1 (n-1))

let rec factorial2 :int -> int = fun n ->
  match( n <= 1) with 
    true -> 1
  |
    false -> n*(factorial2 (n-1))

let rec factorial3 :int -> int = fun n ->
  match n with 
    0 -> 1
  |1 ->1
  |x -> x*(factorial3 (x-1))

let rec concat (x:int list)(y:int list): int list =
  match x with
    [] -> y (* *)
  |
    h::t -> h::(concat t y)(* :: is append h = head t is the rest of the list in this case*)
(*-> fun or pattern matching *)
let rec factorial4(n:int): int = 
  let rec aux n accum = 
    match n with 
      0 -> accum
    |
      1 -> accum
    |
      x -> aux (x-1) (accum*x)
  in aux n 1(*in defines the function call locally with a base case *)
(*match _ with is lik a massive if and the | is or going down*)
let rec aux n accum = 
  match n with 
    0 -> accum
  |
    1 -> accum
  |
    x -> aux (x-1) (accum*x)

let rec factorial5(n:int): int = 
  aux n 1


