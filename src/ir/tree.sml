(* Used code from Tiger Compiler Implementation Using ML textbook *)
structure Tree : TREE = 
struct
    datatype exp = CONST of int
                | NAME of TEMP.label
                | TEMP of TEMP.temp
                | BINOP of binop * exp * exp
                | MEM of exp
                | CALL of exp * exp list
                | ESEQ of stm * exp

    and stm = MOVE of exp * exp
                | EXP of exp
                | JUMP of exp * TEMP.label list
                | CJUMP of relop * exp * exp * TEMP.label * TEMP.label
                | SEQ of stm * stm
                | LABEL of TEMP.label

    and binop = PLUS | MINUS | MUL | DIV 
        | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

    and relop = EQ | NE | LT | GT | LE | GE 
        | ULT | ULE | UGT | UGE

    fun notRel oper = case oper of              (*for CJUMP*)
                    EQ => NE
                    | NE => EQ
                    | GT => LE
                    | LT => GE
                    | GE => LT
                    | LE => GT
                    | ULT => UGE
                    | ULE => UGT
                    | UGT => ULE
                    | UGE => ULT
end