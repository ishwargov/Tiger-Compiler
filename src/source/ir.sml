structure IR : sig
  type inst = (Mips.Label, Temp.temp) MIPS.Inst
  type stmt = (Mips.Label, Temp.temp) MIPS.Stmt
  type prog = stmt list
  val ppInst : inst -> string
  val ppStmt : stmt -> string
  val pp     : prog -> string
end = struct
    val ppInst x = MIPS.prInst(x)
    val ppStmt x = MIPS.prStmt(x)
    val pp [] = "\n"
      | pp (x::xs) = ppStmt(x)^"\n"^pp(xs)
end