structure TIGER = 
struct 

    datatype Binop = Plus
                    | Minus
                    | Div
                    | Mul
                    | And

    datatype Exp =    Val of Int
                    | Var of string
                    | Exp of Exp * Binop * Exp


    datatype Stmt =   Assign of string * Exp
                    | Print of Exp

    datatype Prog = Insts of Stm list

end