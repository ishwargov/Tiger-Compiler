signature GRAPH = sig

type node
type 'a graph

(*
   Create a new node in the graph
   This operation is not a pure function.
*)
val empty   : unit -> 'a graph
val newNode : 'a graph -> 'a  -> node

(* If the data structure was supposed to be persistent the definition
   of new will not be of this form

   addNode : graph -> graph * node
   addEdge : graph -> node * node -> graph
*)


val addEdge : (node * node) -> unit

(* addEdge (a,b) should add and edge starting from a to b *)

val succ    : 'a graph -> node -> node list
val pred    : 'a graph -> node -> node list
val label   : 'a graph -> node -> 'a

val clear   : 'a graph -> unit
val all     : 'a graph -> node list

(* you might want functions that go over all the nodes

maps, folds etc
*)
val map_g : 'a graph -> ('a -> 'b) -> 'b graph

end