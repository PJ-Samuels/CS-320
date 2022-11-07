let list_of_int_as_string l = 
  let f (idx,str) i = 
    let nextStr = if idx = 1 then str^string_of_int i 
      else str^string_of_int i ^"; " 
    in ((idx-1), nextStr)
  in let(_,elems) = List.fold_left f (List.length l, "") l in
  "["^elems^"]" (* has trailing semi colon*)

let _ = print_endline ( list_of_int_as_string [1;2;3;4])