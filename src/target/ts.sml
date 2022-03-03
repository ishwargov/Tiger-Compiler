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

fun compileStmt (Vmap:Env) (TIGER.Assign(x,e)) = let
                                        val t1 = TEMP.newTemp()
                                      in (Vmap := AtomMap.insert(!Vmap,(Atom.atom x),t1); tAssign Vmap t1 e)
                                      end
  | compileStmt (Vmap:Env) (TIGER.Print(e)) = tPrint Vmap (TEMP.newTemp()) e


fun compile []        = []
  | compile (x :: xs) = (map MIPS.Instr (compileStmt Vmap x)) @ (compile xs)
end

(*
x = 5
y = x


li t0 5
li t1 5

*)