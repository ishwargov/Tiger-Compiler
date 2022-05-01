(* Direct Translation to MIPS from Tiger. IR is technically MIPS *)
structure IR : sig
  type inst = (MIPS.Label, MIPS.Reg) MIPS.Inst
  type stmt = (MIPS.Label, MIPS.Reg) MIPS.Stmt
  type prog = stmt list
  val ppInst : inst -> string
  val ppStmt : stmt -> string
  val pp     : prog -> string
end = struct
  type inst = (MIPS.Label, MIPS.Reg) MIPS.Inst
  type stmt = (MIPS.Label, MIPS.Reg) MIPS.Stmt
  type prog = stmt list
  fun ppInst (x:inst) = MIPS.prInst(x)
  fun ppStmt (x:stmt) = MIPS.prStmt(x)
  fun pp ([]:prog) = "\n"
    | pp (x::xs) = ppStmt(x)^pp(xs)
end