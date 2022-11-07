type 'a tree =
  | Leaf of 'a 
  | Node of 'a tree * 'a tree



(*
write a map function for trees:

For example,
let _ = map_tree (fun x -> x+1) (Node (Leaf 1, Leaf 2)) =  (Node (Leaf 2, Leaf 3))
let _ = map_tree (fun _ -> 0)  (Node (Node (Leaf true, Node (Leaf true, Leaf false)), Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false))) =
                       (Node (Node (Leaf 0   , Node (Leaf 0   , Leaf 0    )), Node (Node (Leaf 0   , Node (Leaf 0   , Leaf 0    )), Leaf 0    )))
*)
let rec map_tree (f: 'a -> 'b) (tree: 'a tree): 'b tree  = 
  match tree with
  |Leaf x -> Leaf( f x)
  |Node (x,y) -> Node ((map_tree f x),(map_tree f y))

(*write a fold function for trees:
  For example,
  let _ = fold_tree ( * ) (fun x -> x) (Node (Leaf 3, Leaf 2)) = 6
  let _ = fold_tree (+) (fun _ -> 1) (Node (Node (Leaf true, Node (Leaf true, Leaf false)), Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false))) = 7
*)

let rec fold_tree (node: 'b -> 'b -> 'b)  (leaf: 'a -> 'b)  (tree: 'a tree): 'b  = 
  match tree with
  |Leaf x ->  leaf x
  |Node (x,y) -> node (fold_tree node leaf x )(fold_tree node leaf y)

(* *)
(*
sum the contents of an int tree

For example,
let _ = sum_ints (Node (Leaf 1, Leaf 2)) = 3
*)
let rec sum_ints (tree: int tree): int  =
  match tree with 
  |Leaf x -> x
  |Node(x,y) -> fold_tree (+) (fun x -> x) tree


(*
find the size of the tree

For example,
let _  = tree_size (Node(Leaf 0,Leaf 0))
let _ = tree_size (Leaf 1) = 1
let _ = tree_size (Node (Leaf 1, Leaf 2)) = 3
*)
let rec tree_size  (tree: 'a tree): int  = 
  match tree with
  |Leaf _ -> 1
  |Node(x,y) -> (tree_size x)+(tree_size y)+1

(*
find the height of the tree

For example,
tree_height (Leaf 2) = 1
let _ = tree_height (Node ((Node (Leaf 1, (Node ((Node (Leaf 1, Leaf 2)), Leaf 2)))), Leaf 2)) = 5
*)
let rec tree_height (tree: 'a tree): int  = 
  match tree with
  | Leaf _ -> 1
  | Node(x,y) -> 1 + max(tree_height x) (tree_height y)
(*
For example,
let _  = tree_contains (Node (Leaf 1, Leaf 2)) (fun x -> match x with Leaf 2 -> true | _ -> false) = true
tree_contains (Node (Leaf 1, (Node ((Node (Leaf 1, Leaf 2)), Leaf 2)))) (fun x -> tree_height x > 2) = true
*)
let rec tree_contains (tree: 'a tree) (look_for: 'a tree -> bool): bool  = 
  if look_for tree then true else
    match tree with
    |Leaf _ -> false
    |Node(x,y) -> (tree_contains x look_for) || (tree_contains y look_for)

  (*
write a function that shows bool trees :

For example,
let _ = show_bool_tree (Leaf true) ="true"
let _ = show_bool_tree (Node (Leaf true, Leaf false)) = "(true^false)" 
let _ = show_bool_tree  (Node (Node (Leaf true, Node (Leaf true, Leaf false)),
   Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false))) =
    "((true^(true^false))^((true^(true^false))^false))" 
*)

let rec show_bool_tree (tree: bool tree): string = 
  match tree with
  |Leaf true -> "true"
  |Leaf false -> "false"
  |Node(x,y) -> "("^(show_bool_tree x)^"^"^(show_bool_tree y)^")"


(* standard functions to convert between string and char list *)
let explode s = List.of_seq (String.to_seq s)
let implode cs = String.of_seq (List.to_seq cs)

(*
write a function that reads bool trees :
for all (finite) t : bool trees.
read_bool_tree t = Some (show_bool_tree t)

For example,
let _ = read_bool_tree "true" = Some (Leaf true)
let _ = read_bool_tree "false" = Some (Leaf false)
let _ = read_bool_tree "tralse" = None
let _ = read_bool_tree "(true^false)" = Some (Node (Leaf true, Leaf false))
read_bool_tree "((true^(true^false))^((true^(true^false))^false))" =
Some
 (Node (Node (Leaf true, Node (Leaf true, Leaf false)),
   Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false)))
*)

(* Hint 
   write a helper function 
   read_bool_prefix : (char list) -> ((bool * (char list)) option) 

   such that
   read_bool_prefix (explode "true???")       = Some (true, ['?'; '?'; '?'])
   read_bool_prefix (explode "false123")      = Some (false, ['1'; '2'; '3'])
   read_bool_prefix (explode "antythingales") = None
   read_bool_prefix []                        = None

   write a helper function 
   read_bool_tree_prefix (char list) -> ((bool tree * (char list)) option) 

   such that
   read_bool_tree_prefix [] = None
   read_bool_tree_prefix (explode "true???") = Some (Leaf true, ['?'; '?'; '?'])
   read_bool_tree_prefix (explode "(true^false)124") = Some (Node (Leaf true, Leaf false), ['1'; '2'; '4'])
   read_bool_tree_prefix (explode "(true^(true^false))aaa") = Some (Node (Leaf true, Node (Leaf true, Leaf false)), ['a'; 'a'; 'a'])
   read_bool_tree_prefix (explode "(true^(true^fa se))aaa") = None
*)

let read_bool_prefix (str: char list) : (bool * (char list)) option =
  match str with
  | 't' :: 'r' :: 'u' :: 'e' :: rest_of_string -> Some( true, rest_of_string)
  | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: rest_of_string -> Some(false, rest_of_string)
  | _ -> None



let rec read_bool_tree_prefix (str: char list) : (bool tree * (char list)) option =
  match str with
  | [] -> None
  | h::t -> let result = (
      if h = '(' then (
        match (read_bool_tree_prefix t) with
        | None -> None
        | Some (tree, rest) ->
          match rest with
          | [] -> None
          | h2::t -> (if h2 = ')' then Some (tree, t) else None)
      ) else (
        match (read_bool_prefix str) with
        | None -> Some (Leaf false, str)
        | Some (x, rest) -> Some (Leaf x, rest)
      )
    )in match result with
    | None -> None
    | Some (tempt, rest) ->
      match rest with
      | '^'::tail -> (
          match (read_bool_tree_prefix tail) with
          | None -> None
          | Some (tempt2, rest2) -> Some (Node (tempt, tempt2), rest2)
        )
      | _ -> result


let rec read_bool_tree (tree: string) : ((bool tree) option) = 
  let answer = read_bool_tree_prefix (explode tree) in
  match answer with
  | Some (tree, []) -> Some tree
  | _ -> None

(*
write a fubction that checks that parenthisis are balnaced:
Parenthisis are balenced if there are no parenthises
Parenthisis are balenced if ( and )  enclose a balenced parenthises
Parenthisis are balenced if balenced parenthises are ajacent to a balenced parenthisis

For example,
matching_parens "" = true
matching_parens "((((((((((()))))))))))" = true
matching_parens "()()()()()()" = true
matching_parens "(()())" = true
matching_parens "())(()" = false
*)

(* Hint 
   write mutually recursive functions 
   matching_paren_prefix : (char list) -> ((char list) option)
   matching_parens_prefix : (char list) -> ((char list) option)

   the and keyword allows mutual recursion
   let rec matching_paren_prefix (ls: char list) : ((char list) option) = failwith "unimplemented"
   and matching_parens_prefix  (ls: char list) : ((char list) option) = failwith "unimplemented"

   such that
   matching_paren_prefix [] = None
   matching_paren_prefix (explode "(???") = None
   matching_paren_prefix (explode "()???") = Some ['?'; '?'; '?']
   matching_paren_prefix (explode "(((())))123") = Some ['1'; '2'; '3']
   let _ = matching_paren_prefix (explode "()()()") = Some ['('; ')'; '('; ')']
   matching_paren_prefix (explode "(()()())abc") = Some ['a'; 'b'; 'c']

   matching_parens_prefix [] = Some []
   matching_parens_prefix (explode "()()()") = Some []
   matching_parens_prefix (explode "()())))") = Some [')'; ')'; ')']
   matching_parens_prefix (explode ")aa") = Some [')'; 'a'; 'a']
*)



let rec matching_parens (tree: string) : bool =
  let rec aux tree (accum:int) : bool=
    if accum < 0 then false else 
      match tree with
      | '('::tail -> aux tail (accum+1)
      | ')'::tail ->  aux tail (accum -1)
      | x :: tail -> aux tail accum
      |[] -> if accum = 0 then true else false 
  in aux (explode tree) 0
