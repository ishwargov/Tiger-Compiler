(* For Temp Variable Allocation *)
structure Graph :> GRAPH = struct

type node = int

type 'a graph = {
    labels : 'a AtomTable.hash_table,
    successors : node list AtomTable.hash_table,
    predecessors : node list AtomTable.hash_table,
    nextNode : node ref
}

exception Limit_Exceeded;

fun empty () = {
    labels = AtomTable.mkTable(10,Limit_Exceeded),
    successors = AtomTable.mkTable(10,Limit_Exceeded),
    predecessors = AtomTable.mkTable(10,Limit_Exceeded),
    nextNode = ref 0
}

(* TODO Fix case exhaustive  *)
fun newNode (g : 'a graph) (x: 'a) = let 
                                val _ = ( (#nextNode g) := !(#nextNode g) + 1; 5)
                                val a = AtomTable.insert (#labels g) (Atom.atom (Int.toString(!(#nextNode g))),x)
                                in 
                                !(#nextNode g)
                                end

fun addEdge (g: 'a graph) ((u,v):(node*node) )= let
                                    val a = AtomTable.lookup (#successors g) (Atom.atom (Int.toString(u)))
                                    val b = AtomTable.lookup (#predecessors g) (Atom.atom (Int.toString(v)))
                                    val c = v::a
                                    val d = u::b
                                    val f = AtomTable.insert (#successors g) (Atom.atom (Int.toString(u)),c)
                                    val g = AtomTable.insert (#predecessors g) (Atom.atom (Int.toString(v)),d)
                                in
                                    ()
                                end


fun succ (g: 'a graph) (n:node) = AtomTable.lookup ((#successors g):node list AtomTable.hash_table) (Atom.atom (Int.toString(n)) )
fun pred (g: 'a graph)  (n:node) = AtomTable.lookup ((#predecessors g):node list AtomTable.hash_table) (Atom.atom (Int.toString(n)) )
fun label (g: 'a graph) (n:node) =  AtomTable.lookup ((#labels g): 'a AtomTable.hash_table) (Atom.atom (Int.toString(n)) )

fun clear (g: 'a graph) = let 
                val a = AtomTable.clear ((#successors g):node list AtomTable.hash_table)
                val b = AtomTable.clear  ((#predecessors g):node list AtomTable.hash_table)
                val c = ( ((#nextNode g):int ref) := 0; AtomTable.clear((#labels g): 'a AtomTable.hash_table) )
            in ()
            end

fun all (g: 'a graph)  = let 
                fun get_first [] = []
                  | get_first ((a,b)::xs) = valOf(Int.fromString (Atom.toString(a)) ) :: get_first(xs) 
            in
                get_first(AtomTable.listItemsi((#labels g): 'a AtomTable.hash_table))
            end

end



(*


val tb : int list AtomTable.hash_table = AtomTable.mkTable(5,Limit_Exceeded);
AtomTable.insert tb (Atom.atom "$t1",[1,2,3,4]);  


*)