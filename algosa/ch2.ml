let tab  = [|1;2;7;7;7;12;17;23;42|]

let dicho tab value =
  let rec aux deb fin =
    let milieu = (deb+fin) / 2 in
    if ((fin-deb = 1)) then false else
    if (tab.(milieu) = value) then true else
    if (tab.(milieu) > value) then aux deb milieu else aux milieu fin
  in aux 0 (Array.length tab)


let l = [1;2;3;4;5;6;7;8;9]

let pref l x =
  let rec aux l x acc =
  if (List.length l < x) then failwith "Trop courte" else
  if x <= 0 then acc else
    match l with
    [] -> acc
    |h::t -> aux t (x-1) (acc@[h])
   in aux l x []

let suff l x = List.rev(pref (List.rev l) x)

let rec insert value l =
    match l with
      [] -> [value]
    |h::t -> if value < h then value::h::t  else h::insert value t
 
let rec tri_insertion l  =
  match l with
  | [] -> []
  | h::t -> insert h (tri_insertion t)

let tri_insertion l =
  let rec aux l acc =
    match l with
      [] -> acc
    |h::t -> aux t (insert h acc)
  in aux l []

let split l =
  let rec aux l acc aux lgt =
    if lgt = 0 then
      
