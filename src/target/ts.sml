structure TRANSLATE = 
struct

type Env = TEMP.temp AtomMap.map ref
val (Vmap:Env) = ref AtomMap.empty

fun tAssign (Vmap:Env) (Tmp:TEMP.temp) (Exp:TIGER.Exp) = case Exp of
                    TIGER.Val(x) => [MIPS.Li(TEMP.tempToReg(Tmp),x)]
                  | TIGER.Var(x) => [MIPS.Move( TEMP.tempToReg(Tmp),TEMP.tempToReg(valOf(AtomMap.find(!Vmap,(Atom.atom x) ))))]
                  | TIGER.Exp(e1,TIGER.Plus,e2) => let 
                                                    val u = TEMP.newTemp()
                                                    val v = TEMP.newTemp()
                                                    val l1 = tAssign Vmap u e1
                                                    val l2 = tAssign Vmap v e2
                                                    in (l1 @ l2 @ [MIPS.Add(TEMP.tempToReg(Tmp),TEMP.tempToReg(u),TEMP.tempToReg(v))])
                                                    end
                  | TIGER.Exp(e1,TIGER.Minus,e2) =>  let 
                                                    val u = TEMP.newTemp()
                                                    val v = TEMP.newTemp()
                                                    val l1 = tAssign Vmap u e1
                                                    val l2 = tAssign Vmap v e2
                                                    in (l1 @ l2 @ [MIPS.Sub(TEMP.tempToReg(Tmp),TEMP.tempToReg(u),TEMP.tempToReg(v))])
                                                    end
                  | TIGER.Exp(e1,TIGER.Div,e2) =>  let 
                                                    val u = TEMP.newTemp()
                                                    val v = TEMP.newTemp()
                                                    val l1 = tAssign Vmap u e1
                                                    val l2 = tAssign Vmap v e2
                                                    in (l1 @ l2 @ [MIPS.Divi(TEMP.tempToReg(Tmp),TEMP.tempToReg(u),TEMP.tempToReg(v))])
                                                    end
                  | TIGER.Exp(e1,TIGER.Mul,e2) =>  let 
                                                    val u = TEMP.newTemp()
                                                    val v = TEMP.newTemp()
                                                    val l1 = tAssign Vmap u e1
                                                    val l2 = tAssign Vmap v e2
                                                    in (l1 @ l2 @ [MIPS.Mul(TEMP.tempToReg(Tmp),TEMP.tempToReg(u),TEMP.tempToReg(v))])
                                                    end

fun tPrint (Vmap:Env) (Tmp:TEMP.temp) (Exp:TIGER.Exp) = case Exp of 
                  TIGER.Val(x) => [MIPS.Li(MIPS.A(0),x),MIPS.Li(MIPS.V(0),4),MIPS.Syscall] 
                | TIGER.Var(x) => [MIPS.Move(MIPS.A(0),TEMP.tempToReg(valOf(AtomMap.find(!Vmap,(Atom.atom x) )))),MIPS.Li(MIPS.V(0),4),MIPS.Syscall] 
                | TIGER.Exp(e1,TIGER.Plus,e2) => let 
                                                    val u = TEMP.newTemp()
                                                    val v = TEMP.newTemp()
                                                    val l1 = tAssign Vmap u e1
                                                    val l2 = tAssign Vmap v e2
                                                    in (l1 @ l2 @ [MIPS.Add(TEMP.tempToReg(Tmp),TEMP.tempToReg(u),TEMP.tempToReg(v)),MIPS.Move(MIPS.A(0),TEMP.tempToReg(Tmp)),MIPS.Li(MIPS.V(0),4),MIPS.Syscall])
                                                    end
                  | TIGER.Exp(e1,TIGER.Minus,e2) =>  let 
                                                    val u = TEMP.newTemp()
                                                    val v = TEMP.newTemp()
                                                    val l1 = tAssign Vmap u e1
                                                    val l2 = tAssign Vmap v e2
                                                    in (l1 @ l2 @ [MIPS.Sub(TEMP.tempToReg(Tmp),TEMP.tempToReg(u),TEMP.tempToReg(v)),MIPS.Move(MIPS.A(0),TEMP.tempToReg(Tmp)),MIPS.Li(MIPS.V(0),4),MIPS.Syscall])
                                                    end
                  | TIGER.Exp(e1,TIGER.Div,e2) =>  let 
                                                    val u = TEMP.newTemp()
                                                    val v = TEMP.newTemp()
                                                    val l1 = tAssign Vmap u e1
                                                    val l2 = tAssign Vmap v e2
                                                    in (l1 @ l2 @ [MIPS.Divi(TEMP.tempToReg(Tmp),TEMP.tempToReg(u),TEMP.tempToReg(v)),MIPS.Move(MIPS.A(0),TEMP.tempToReg(Tmp)),MIPS.Li(MIPS.V(0),4),MIPS.Syscall])
                                                    end
                  | TIGER.Exp(e1,TIGER.Mul,e2) =>  let 
                                                    val u = TEMP.newTemp()
                                                    val v = TEMP.newTemp()
                                                    val l1 = tAssign Vmap u e1
                                                    val l2 = tAssign Vmap v e2
                                                    in (l1 @ l2 @ [MIPS.Mul(TEMP.tempToReg(Tmp),TEMP.tempToReg(u),TEMP.tempToReg(v)),MIPS.Move(MIPS.A(0),TEMP.tempToReg(Tmp)),MIPS.Li(MIPS.V(0),4),MIPS.Syscall])
                                                    end

fun compileStmt (Vmap:Env) (TIGER.Assign(x,e)) = let
                                        val t1 = TEMP.newTemp()
                                      in (Vmap := AtomMap.insert(!Vmap,(Atom.atom x),t1); map MIPS.Instr (tAssign Vmap t1 e))
                                      end
  | compileStmt (Vmap:Env) (TIGER.Print(e)) = map MIPS.Instr (tPrint Vmap (TEMP.newTemp()) e)
  | compileStmt (Vmap:Env) (TIGER.For(i,v1,v2,st)) = let 
                                                          val l1 = TEMP.newLabel()
                                                          val l2 = TEMP.newLabel()
                                                          val t1 = TEMP.newTemp()
                                                          val t2 = TEMP.newTemp()
                                                          val Vmap_new = ref (AtomMap.insert(!Vmap,(Atom.atom i),t1))
                                                          val s1 = map MIPS.Instr (tAssign Vmap_new t1 (TIGER.Val(v1)))
                                                          val s2 = map MIPS.Instr (tAssign Vmap_new t2 (TIGER.Val(v2)))
                                                          fun compileStmtLis (Vmap:Env) (x::xs) = (compileStmt Vmap x) @ (compileStmtLis Vmap xs)
                                                          val s3 = compileStmtLis Vmap_new st
                                                          in (
                                                              s1
                                                            @ s2
                                                            @ [MIPS.LabelStmt(TEMP.labelToLabel(l1))]
                                                            @ [MIPS.Instr(MIPS.Bgt(TEMP.tempToReg(t1),TEMP.tempToReg(t2),TEMP.labelToLabel(l2)))]
                                                            @ s3
                                                            @ [MIPS.Instr(MIPS.Addi(TEMP.tempToReg(t1),TEMP.tempToReg(t1),1))]
                                                            @ [MIPS.Instr(MIPS.J(TEMP.labelToLabel(l1)))]
                                                            @ [MIPS.LabelStmt(TEMP.labelToLabel(l2))]
                                                          ) 
                                                          end



fun compile []        = []
  | compile (x :: xs) = (compileStmt Vmap x) @ (compile xs)
end

(*
x = 5
y = x


li t0 5
li t1 5

*)