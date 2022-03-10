structure TIGER = 
struct 

    datatype Binop = Plus
                    | Minus
                    | Div
                    | Mul

    datatype Exp =    Val of int
                    | Var of string
                    | Exp of Exp * Binop * Exp


    datatype Stmt =   Assign of string * Exp
                    | Print of Exp
                    | For of Var * Val * Val * Stmt list


end