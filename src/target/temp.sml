signature TEMPS =
  sig
     type temp
     val newTemp : unit -> temp
     val tempToString : temp -> string
     val tempToReg : temp -> MIPS.Reg
  end

structure TEMP :> TEMPS = struct

   type temp  = int                                (* 2ʷ many variables on a w-sized machine *)
		                                             (* you can use IntInf.int if you want unbounded *)
   val nextTemp       = ref 0                      (* Keep track of how many temps have been allocated *)
   fun newTemp ()     = let val tmp = !nextTemp in (nextTemp := !nextTemp+1; tmp) end
   fun tempToString t = "$t"^Int.toString(t)
   fun tempToReg t = MIPS.T(t)
end