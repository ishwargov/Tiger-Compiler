signature GRAPH = sig

type node
type 'a graph

val empty   : unit -> 'a graph
val newNode : 'a graph -> 'a  -> node

val addEdge : 'a graph -> (node * node) -> unit                 (*   Updated Sir's definition       *)

val succ    : 'a graph -> node -> node list
val pred    : 'a graph -> node -> node list
val label   : 'a graph -> node -> 'a

val clear   : 'a graph -> unit
val all     : 'a graph -> node list

(*
val map_g : 'a graph -> ('a -> 'b) -> 'b graph              (* added map and fold*)
val fold_g: ('a * 'b -> 'b) -> 'b -> 'a graph -> 'b 
*)
end