type 'a list =
  Empty
| Cell of 'a * 'a list

type 'a bin =
  BE
| Node of 'a * 'a bin * 'a bin

let tree = Node(1, Node(2, BE, BE), Node(3, BE, Node(4, BE, BE)))

let rec heigth tree =
  match tree with
    BE -> 0
  | Node(_,BE,BE) -> 0
  | Node(_,l,r) -> 1 + max (heigth l) (heigth r)

let tree = Node (1, Node (2, BE, BE), Node (3, BE, Node (4, BE, BE)))

let rec bst_search e bst =
  match bst with
    BE -> false
  | Node (x, l, r) -> if x = e then
                        true
                      else
                        bst_search e (if e < x then l else r)


type ('k,'v) bst = |Key of 'k * 'v

type 'k bst_multiset =
  |Empty
  |Multiset of 'k bst_multiset * ('k * int) * 'k bst_multiset

let example36 = Multiset(Multiset(Multiset(Empty,(15,1),Empty),(17,1),Empty),(42,2),Multiset(Multiset(Empty,(45,1),Empty),(84,1),Multiset(Empty,(99,1),Empty)))

let rec bst_search tree value =
  match tree with
  |Empty -> 0
  |Multiset(l,(x,m),r) -> if value = x then m else bst_search (if value < x then l else r ) value

let rec list_of_bst tree =
  let rec aux tree acc =
    match tree with
    |Empty -> acc
    |Multiset(g,(x,m),d) -> aux g acc @  inse x m  @ aux d acc
  in aux tree []

let insert_list x nb =
  let rec aux nb acc =
    if nb >0 then aux (nb-1) (x::acc) else acc
  in aux nb []
