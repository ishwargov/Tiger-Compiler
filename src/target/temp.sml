signature TEMPS =
  sig
     type temp
     type label
     val newTemp : unit -> temp
     val newLabel : unit -> label
     val tempToString : temp -> string
     val tempToReg : temp -> MIPS.Reg
     val labelToString : label ->  string
     val labelToLabel : label -> MIPS.Label
  end

structure TEMP :> TEMPS = struct

   type temp  = int                                (* 2ʷ many variables on a w-sized machine *)
   type label = int
		                                             (* you can use IntInf.int if you want unbounded *)
   val nextTemp       = ref 0                      (* Keep track of how many temps have been allocated *)
   val nextLabel      = ref 0
   fun newTemp ()     = let val tmp = !nextTemp in (nextTemp := !nextTemp+1; tmp) end
   fun newLabel ()    = let val lab = !nextLabel in (nextLabel := !nextLabel+1; lab) end
   fun tempToString t = "$t"^Int.toString(t)
   fun labelToString l = "$L"^Int.toString(l)
   fun tempToReg t = MIPS.T(t)                                                (* TODO : Change to Graph coloring and Register Allocation*)
   fun labelToLabel l = MIPS.TempLabel(l)
end