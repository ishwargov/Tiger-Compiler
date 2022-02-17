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
                      | Bgezal of 't * 'l 
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
                      | Lwcz of 't * 'l
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

datatype ('l,'t) Stmt = Align of int
                      | Ascii of string
                      | Asciiz of string
                      | Byte of int list
                      | Data 
                      | Extern of int * int
                      | Global of int 
                      | Half of int list
                      | Kdata
                      | Ktext
                      | Space of int
                      | Text
                      | word of int list


end