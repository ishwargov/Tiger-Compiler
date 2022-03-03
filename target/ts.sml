structure TRANSLATE = 
struct
fun tAssign (Tmp:TEMP.temp) (Exp:TIGER.Exp) = case Exp of
                    TIGER.Val(x) = [MIPS.Li(Tmp,x)]
                  | TIGER.Var(x) = [MIPS.Li(Tmp,0)]
                  | TIGER.Exp(e1,TIGER.Plus,e2) = let 
                                                    val u = TEMP.newTemp()
                                                    val v = TEMP.newTemp()
                                                    val l1 = tAssign u e1
                                                    val l2 = tAssign v e2
                                                    in l1 @ l2 @ [MIPS.Add(TEMP.newTemp(),u,v)]
                  | TIGER.Exp(e1,TIGER.Minus,e2) =  let 
                                                    val u = TEMP.newTemp()
                                                    val v = TEMP.newTemp()
                                                    val l1 = tAssign u e1
                                                    val l2 = tAssign v e2
                                                    in l1 @ l2 @ [MIPS.Sub(TEMP.newTemp(),u,v)]
                  | TIGER.Exp(e1,TIGER.Div,e2) =  let 
                                                    val u = TEMP.newTemp()
                                                    val v = TEMP.newTemp()
                                                    val l1 = tAssign u e1
                                                    val l2 = tAssign v e2
                                                    in l1 @ l2 @ [MIPS.Div(TEMP.newTemp(),u,v)]
                  | TIGER.Exp(e1,TIGER.Mul,e2) =  let 
                                                    val u = TEMP.newTemp()
                                                    val v = TEMP.newTemp()
                                                    val l1 = tAssign u e1
                                                    val l2 = tAssign v e2
                                                    in l1 @ l2 @ [MIPS.Mul(TEMP.newTemp(),u,v)]
                  | TIGER.Exp(e1,TIGER.And,e2) =  let 
                                                    val u = TEMP.newTemp()
                                                    val v = TEMP.newTemp()
                                                    val l1 = tAssign u e1
                                                    val l2 = tAssign v e2
                                                    in l1 @ l2 @ [MIPS.And(TEMP.newTemp(),u,v)]

fun tPrint (Tmp:TEMP.temp) (Exp:TIGER.Exp) = case Exp of 
                  TIGER.Val(x) = [MIPS.Li(MIPS.A(0),x),MIPS.Li(MIPS.V(0),4),MIPS.Syscall]   
     


     
end

(*
x = 5
y = x


li t0 5
li t1 5

*)