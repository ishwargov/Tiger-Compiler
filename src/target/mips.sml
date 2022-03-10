structure MIPS = 
struct 

datatype Reg =  Zero
             |  V of int
             |  A of int
             |  T of int
             |  S of int
             |  GP
             |  SP
             |  FP
             |  RA

datatype Label = UserDefined of string 
               | TempLabel   of int 

datatype ('l,'t) Inst = Abs of 't * 't                     
                      | Add of 't * 't * 't
                      | Addi of 't * 't * int
                      | Addu of 't * 't * 't
                      | Addiu of 't * 't * int                     
                      | And of 't * 't * 't
                      | Andi of 't * 't * int                      
                      | Div of 't * 't 
                      | Divu of 't * 't 
                      | Divi of 't * 't * 't
                      | Diviu of 't * 't * 't                    
                      | Mul of 't * 't * 't
                      | Mulo of 't * 't * 't
                      | Mulou of 't * 't * 't
                      | Mult of 't * 't 
                      | Multu of 't * 't
                      | Neg of 't * 't 
                      | Negu of 't * 't 
                      | Nor of 't * 't * 't
                      | Not of 't * 't
                      | Or of 't * 't * 't
                      | Ori of 't * 't * int
                      | Rem of 't * 't * 't
                      | Remu of 't * 't * 't
                      | Rol of 't * 't * 't
                      | Ror of 't * 't * 't
                      | Sll of 't * 't * 't
                      | Sllv of 't * 't * 't
                      | Sra of 't * 't * 't
                      | Srav of 't * 't * 't
                      | Srl of 't * 't * 't
                      | Srlv of 't * 't * 't
                      | Sub of 't * 't * 't
                      | Subu of 't * 't * 't
                      | Xor of 't * 't * 't
                      | Xori of 't * 't * int

                      | Li of 't * int
                      | Lui of 't * int

                      | Seq of 't * 't * 't
                      | Sge of 't * 't * 't
                      | Sgeu of 't * 't * 't
                      | Sgt of 't * 't * 't
                      | Sgtu of 't * 't * 't
                      | Sle of 't * 't * 't
                      | Sleu of 't * 't * 't
                      | Slt of 't * 't * 't
                      | Slti of 't * 't * int
                      | Sltu of 't * 't * 't
                      | Sltiu of 't * 't * int
                      | Sne of 't * 't * 't

                      | B of 'l
                      | Bczt of 'l
                      | Bczf of 'l
                      | Beq of 't * 't * 'l
                      | Beqz of 't * 'l
                      | Bge of 't * 't * 'l 
                      | Bgeu of 't * 't * 'l 
                      | Bgez of 't * 'l 
                      | Bgt of 't * 't * 'l 
                      | Bgtu of 't * 't * 'l 
                      | Bgtz of 't * 'l
                      | Ble of 't * 't * 'l
                      | Bleu of 't * 't * 'l
                      | Blez of 't * 'l
                      | Bltzal of 't * 'l
                      | Blt of 't * 't * 'l
                      | Bltu of 't * 't * 'l
                      | Bltz of 't * 'l
                      | Bne of 't * 't * 'l
                      | Bnez of 't * 'l
                      | J of 'l
                      | Jal of 'l
                      | Jalr of 't
                      | Jr of 't

                      | La of 't * 'l
                      | Lb of 't * 'l
                      | Lbu of 't * 'l
                      | Ld of 't * 'l
                      | Lh of 't * 'l
                      | Lhu of 't * 'l
                      | Lw of 't * 'l
                      | Lwl of 't * 'l
                      | Lwr of 't * 'l
                      | Ulh of 't * 'l
                      | Ulhu of 't * 'l
                      | Ulw of 't * 'l

                      | Sb of 't * 'l
                      | Sd of 't * 'l
                      | Sh of 't * 'l
                      | Sw of 't * 'l
                      | Swl of 't * 'l
                      | Swr of 't * 'l
                      | Ush of 't * 'l
                      | Usw of 't * 'l

                      | Move of 't * 't
                      | Mfhi of 't
                      | Mflo of 't
                      | Mthi of 't
                      | Mtlo of 't

                      | Rfe
                      | Syscall
                      | Break of int
                      | Nop

datatype ('l,'t) Stmt = Instr of ('l,'t) Inst
                      | Align of int
                      | Ascii of string
                      | Asciiz of string
                      | Byte of int list
                      | Data 
                      | Extern of int * int
                      | Global of string 
                      | Half of int list
                      | Kdata
                      | Ktext
                      | Space of int
                      | Text
                      | Word of int list
                      | LabelStmt of Label


fun prReg r = case r of 
                Zero => "$zero"
              | GP => "$gp"
              | SP => "$sp"
              | FP => "$fp"
              | RA => "$ra"
              | V(x) => "$v"^Int.toString(x)
              | A(x) => "$a"^Int.toString(x)
              | T(x) => "$t"^Int.toString(x)
              | S(x) => "$s"^Int.toString(x) 

(* fun prReg t = "$t"^Int.toString(t) *)


fun prLabel (l:Label) = case l of
                        UserDefined(x) => x
                      | TempLabel(x) => "$L"^Int.toString(x)

fun prInst(i : (Label,Reg) Inst) = case i of
                                        Abs(t1,t2) => "abs "^prReg(t1)^", "^prReg(t2)
                                      | Add(t1,t2,t3) => "add "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Addi(t1,t2,imm) => "addi "^prReg(t1)^", "^prReg(t2)^", "^Int.toString(imm)
                                      | Addu(t1,t2,t3) => "addu "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Addiu(t1,t2,imm) => "addiu "^prReg(t1)^", "^prReg(t2)^", "^Int.toString(imm)
                                      | And(t1,t2,t3) => "and "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Andi(t1,t2,imm) => "andi "^prReg(t1)^", "^prReg(t2)^", "^Int.toString(imm)
                                      | Div(t1,t2) => "div "^prReg(t1)^", "^prReg(t2)
                                      | Divu(t1,t2) => "divu "^prReg(t1)^", "^prReg(t2)
                                      | Divi(t1,t2,t3) => "div "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Diviu(t1,t2,t3) => "divu "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Mul(t1,t2,t3) => "mul "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Mulo(t1,t2,t3) => "mulo "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Mulou(t1,t2,t3) => "mulou "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Mult(t1,t2) => "mult "^prReg(t1)^", "^prReg(t2)
                                      | Multu(t1,t2) => "multu "^prReg(t1)^", "^prReg(t2)
                                      | Neg(t1,t2) => "neg "^prReg(t1)^", "^prReg(t2)
                                      | Negu(t1,t2) => "negu "^prReg(t1)^", "^prReg(t2)
                                      | Nor(t1,t2,t3) => "nor "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Not(t1,t2) => "abs "^prReg(t1)^", "^prReg(t2)
                                      | Or(t1,t2,t3) => "or "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Ori(t1,t2,imm) => "ori "^prReg(t1)^", "^prReg(t2)^", "^Int.toString(imm)
                                      | Rem(t1,t2,t3) => "rem "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Remu(t1,t2,t3) => "remu "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Rol(t1,t2,t3) => "rol "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Ror(t1,t2,t3) => "ror "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Sll(t1,t2,t3) => "sll "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Sllv(t1,t2,t3) => "sllv "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Sra(t1,t2,t3) => "sra "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Srav(t1,t2,t3) => "srav "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Srl(t1,t2,t3) => "srl "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Srlv(t1,t2,t3) => "srlv "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Sub(t1,t2,t3) => "sub "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Subu(t1,t2,t3) => "subu "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Xor(t1,t2,t3) => "xor "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Xori(t1,t2,imm) => "xori "^prReg(t1)^", "^prReg(t2)^", "^Int.toString(imm)
                                      
                                      | Li(t1,imm) => "li "^prReg(t1)^", "^Int.toString(imm)
                                      | Lui(t1,imm) => "lui "^prReg(t1)^", "^Int.toString(imm)

                                      | Seq(t1,t2,t3) => "seq "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Sge(t1,t2,t3) => "sge "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Sgeu(t1,t2,t3) => "sgeu "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Sgt(t1,t2,t3) => "sgt "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Sgtu(t1,t2,t3) => "sgtu "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Sle(t1,t2,t3) => "sle "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Sleu(t1,t2,t3) => "sleu "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Slt(t1,t2,t3) => "slt "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Slti(t1,t2,imm) => "slti "^prReg(t1)^", "^prReg(t2)^", "^Int.toString(imm)
                                      | Sltu(t1,t2,t3) => "sltu "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)
                                      | Sltiu(t1,t2,imm) => "sltiu "^prReg(t1)^", "^prReg(t2)^", "^Int.toString(imm)
                                      | Sne(t1,t2,t3) => "sne "^prReg(t1)^", "^prReg(t2)^", "^prReg(t3)

                                      | B(l1) => "b "^prLabel(l1)
                                      | Bczt(l1) => "bczt "^prLabel(l1)
                                      | Bczf(l1) => "bczf "^prLabel(l1)
                                      | Beq(t1,t2,l1) => "beq "^prReg(t1)^", "^prReg(t2)^", "^prLabel(l1)
                                      | Beqz(t1,l1) => "beqz "^prReg(t1)^", "^prLabel(l1)
                                      | Bge(t1,t2,l1) => "bge "^prReg(t1)^", "^prReg(t2)^", "^prLabel(l1)
                                      | Bgeu(t1,t2,l1) => "bgeu "^prReg(t1)^", "^prReg(t2)^", "^prLabel(l1)
                                      | Bgez(t1,l1) => "bgez "^prReg(t1)^", "^prLabel(l1)
                                      | Bgt(t1,t2,l1) => "bgt "^prReg(t1)^", "^prReg(t2)^", "^prLabel(l1)
                                      | Bgtu(t1,t2,l1) => "bgtu "^prReg(t1)^", "^prReg(t2)^", "^prLabel(l1)
                                      | Bgtz(t1,l1) => "bgtz "^prReg(t1)^", "^prLabel(l1)
                                      | Ble(t1,t2,l1) => "ble "^prReg(t1)^", "^prReg(t2)^", "^prLabel(l1)
                                      | Bleu(t1,t2,l1) => "bleu "^prReg(t1)^", "^prReg(t2)^", "^prLabel(l1)
                                      | Blez(t1,l1) => "blez "^prReg(t1)^", "^prLabel(l1)
                                      | Bltzal(t1,l1) => "bltzal "^prReg(t1)^", "^prLabel(l1)
                                      | Blt(t1,t2,l1) => "blt "^prReg(t1)^", "^prReg(t2)^", "^prLabel(l1)
                                      | Bltu(t1,t2,l1) => "bltu "^prReg(t1)^", "^prReg(t2)^", "^prLabel(l1)
                                      | Bltz(t1,l1) => "bltz "^prReg(t1)^", "^prLabel(l1)
                                      | Bne(t1,t2,l1) => "bne "^prReg(t1)^", "^prReg(t2)^", "^prLabel(l1)
                                      | Bnez(t1,l1) => "bnez "^prReg(t1)^", "^prLabel(l1)
                                      | J(l1) => "j "^prLabel(l1)
                                      | Jal(l1) => "jal "^prLabel(l1)
                                      | Jalr(t1) => "jalr "^prReg(t1)
                                      | Jr(t1) => "jr "^prReg(t1)

                                      | La(t1,l1) => "la "^prReg(t1)^", "^prLabel(l1)
                                      | Lb(t1,l1) => "lb "^prReg(t1)^", "^prLabel(l1)
                                      | Lbu(t1,l1) => "lbu "^prReg(t1)^", "^prLabel(l1)
                                      | Ld(t1,l1) => "ld "^prReg(t1)^", "^prLabel(l1)
                                      | Lh(t1,l1) => "lh "^prReg(t1)^", "^prLabel(l1)
                                      | Lhu(t1,l1) => "lhu "^prReg(t1)^", "^prLabel(l1)
                                      | Lw(t1,l1) => "lw "^prReg(t1)^", "^prLabel(l1)
                                      | Lwl(t1,l1) => "lwl "^prReg(t1)^", "^prLabel(l1)
                                      | Lwr(t1,l1) => "lwr "^prReg(t1)^", "^prLabel(l1)
                                      | Ulh(t1,l1) => "ulh "^prReg(t1)^", "^prLabel(l1)
                                      | Ulhu(t1,l1) => "ulhu "^prReg(t1)^", "^prLabel(l1)
                                      | Ulw(t1,l1) => "ulw "^prReg(t1)^", "^prLabel(l1)

                                      | Sb(t1,l1) => "sb "^prReg(t1)^", "^prLabel(l1)
                                      | Sd(t1,l1) => "sd "^prReg(t1)^", "^prLabel(l1)
                                      | Sh(t1,l1) => "sh "^prReg(t1)^", "^prLabel(l1)
                                      | Sw(t1,l1) => "sw "^prReg(t1)^", "^prLabel(l1)
                                      | Swl(t1,l1) => "swl "^prReg(t1)^", "^prLabel(l1)
                                      | Swr(t1,l1) => "swr "^prReg(t1)^", "^prLabel(l1)
                                      | Ush(t1,l1) => "ush "^prReg(t1)^", "^prLabel(l1)
                                      | Usw(t1,l1) => "usw "^prReg(t1)^", "^prLabel(l1)

                                      | Move(t1,t2) => "move "^prReg(t1)^", "^prReg(t2)
                                      | Mfhi(t1) => "mfhi "^prReg(t1)
                                      | Mflo(t1) => "mflo "^prReg(t1)
                                      | Mthi(t1) => "mthi "^prReg(t1)
                                      | Mtlo(t1) => "mtlo "^prReg(t1)

                                      | Rfe => "rfe"
                                      | Syscall => "syscall"
                                      | Break(x) => "break "^Int.toString(x)
                                      | Nop => "nop"

fun prLis [] = ""
    | prLis ([x]) = Int.toString(x)
    | prLis (x::xs) = Int.toString(x)^","^prLis(xs)


fun prStmt(stm : (Label,Reg) Stmt) = case stm of 
                                        Instr(x) => prInst(x)^"\n"
                                      | Align(x) => ".align "^Int.toString(x)^"\n"
                                      | Ascii(s) => ".ascii "^s^"\n"  
                                      | Asciiz(s) => ".asciiz "^s^"\n"  
                                      | Byte(lis) => ".byte "^prLis(lis)^"\n"
                                      | Data => ".data \n" 
                                      | Extern(sym,size) => ".extern "^Int.toString(sym)^" "^Int.toString(size)^"\n"
                                      | Global(sym) => ".globl "^sym^"\n"
                                      | Half(lis) => ".half "^prLis(lis)^"\n"
                                      | Kdata => ".kdata \n"
                                      | Ktext => ".ktext \n"
                                      | Space(n) => ".space "^Int.toString(n)^"\n"
                                      | Text => ".text \n"
                                      | Word(lis) => ".word "^prLis(lis)^"\n"
                                      | LabelStmt(x) => prLabel(x)^" :\n"


end