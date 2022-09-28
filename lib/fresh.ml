open Base

type t = {
  base : string ;
  counter : int ;
  forbid : (string , String.comparator_witness) Set.t
}


let init ~base ~forbid = {base = base ; counter = 0 ; forbid = Set.of_list (module String) forbid}

let rec get {base = b ; counter = c ; forbid = f} : string * t = 
  let name = b ^ (Int.to_string c) in
  let nxt = {base = b ; counter = c + 1 ; forbid = f} in
  if Set.mem f name 
    then get nxt
    else (name , nxt)

