
type bank_db = (string * float) list 
let db : bank_db = 
  [
    ("Jack",100.);
    ("Jill",20.);
    ("Jeff",4500.);
    ("John",99.);
    ("Jess",120.);
    ("Janet",111.);
    ("Jake",213.);
  ]

(*let map (f: a' -> b')(xs: a' list): b' list = *)
let rec query_db( db :bank_db) (name:string) : float = 
  match db with
  | [] -> -1.
  |(client,balance):: db' ->
    if client = name then balance else quary_db db' name

let rec query_db( db :bank_db) (name:string) : float option= 
  match db with
  | [] -> None
  |(client,balance):: db' ->
    if client = name then Some balance else quary_db db' name

let add1(x :int option): int option = 
  match x with 
  |None -> None
  |Some x' -> Some(x' +1)

let rec all_clients (db: bank _db ): string list = 
  match db with 
  | [] -> []
  | (client, balance) :: db' -> client :: all_clients db'

let rec update (db: bank _db )(name: string)(n:float): string list = 
  match db with 
  | [] -> []
  | (client, balance) :: db' -> 
    if client = name then (client,n) :: db' 
    else update db' name
let rec update_all (db: bank _db )(rate:float): bank_db = 
  match db with
  | [] ->[]
  | (client, balance) :: db' -> 
    let balance' = balance *. (1. +. rate)
    in(client, 'balance):: update_all 'db rate


let rec map(f : 'a'-> 'b)(xs: 'a list): 'b list = 
  match xs with 
  |[] ->[]
  | x :: xs' -> f x :: map f xs'

let all_clients (db: bank_db) : string list =
  let f (client, balance) = client 
  in map f db 

let update (db: bank_db) (name: string) (n:float): bank_db = 
  let f (client,balance) = 
    if client = name then (client,n)
    else (client, balance)
  in map f db
let update_all(db:bank_db) (rate: float) bank_db = 
  let f (client, balance) = 
    let balance' = balance *. (1. +. rate)
    in (client, balance')
  in map f db