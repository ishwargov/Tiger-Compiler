functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "tiger.grm"*)(* This is the preamble where you can have arbitrary sml code. For us
it is empty *)


(*#line 15.1 "tiger.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\014\000\002\000\013\000\010\000\012\000\000\000\
\\001\000\001\000\024\000\000\000\
\\001\000\001\000\031\000\000\000\
\\001\000\002\000\010\000\000\000\
\\001\000\003\000\015\000\000\000\
\\001\000\003\000\017\000\000\000\
\\001\000\004\000\021\000\005\000\020\000\006\000\019\000\007\000\018\000\
\\011\000\029\000\000\000\
\\001\000\008\000\000\000\000\000\
\\001\000\009\000\009\000\000\000\
\\001\000\009\000\032\000\000\000\
\\001\000\009\000\034\000\000\000\
\\001\000\014\000\033\000\000\000\
\\001\000\015\000\036\000\000\000\
\\001\000\016\000\030\000\000\000\
\\038\000\009\000\008\000\000\000\
\\039\000\000\000\
\\040\000\002\000\007\000\012\000\006\000\013\000\005\000\000\000\
\\041\000\000\000\
\\042\000\004\000\021\000\005\000\020\000\006\000\019\000\007\000\018\000\000\000\
\\043\000\004\000\021\000\005\000\020\000\006\000\019\000\007\000\018\000\000\000\
\\044\000\000\000\
\\045\000\000\000\
\\046\000\000\000\
\\047\000\000\000\
\\048\000\006\000\019\000\007\000\018\000\000\000\
\\049\000\006\000\019\000\007\000\018\000\000\000\
\\050\000\000\000\
\\051\000\000\000\
\"
val actionRowNumbers =
"\016\000\014\000\008\000\003\000\
\\000\000\004\000\015\000\016\000\
\\005\000\019\000\000\000\022\000\
\\021\000\000\000\017\000\001\000\
\\000\000\000\000\000\000\000\000\
\\006\000\018\000\013\000\027\000\
\\026\000\025\000\024\000\023\000\
\\002\000\009\000\011\000\010\000\
\\016\000\012\000\020\000\007\000"
val gotoT =
"\
\\002\000\002\000\003\000\001\000\004\000\035\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\009\000\000\000\
\\000\000\
\\000\000\
\\002\000\002\000\003\000\014\000\000\000\
\\000\000\
\\000\000\
\\001\000\020\000\000\000\
\\000\000\
\\000\000\
\\001\000\021\000\000\000\
\\000\000\
\\000\000\
\\001\000\023\000\000\000\
\\001\000\024\000\000\000\
\\001\000\025\000\000\000\
\\001\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\002\000\003\000\033\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 36
val numrules = 14
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | VAR of  (string) | CONST of  (int) | PROGRAM of  (TIGER.Stmt list) | STMTS of  (TIGER.Stmt list) | STMT of  (TIGER.Stmt) | EXP of  (TIGER.Exp)
end
type svalue = MlyValue.svalue
type result = TIGER.Stmt list
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 12) => true | (T 13) => true | (T 11) => true | (T 15) => true | (T 14) => true | _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 7) => true | _ => false
val showTerminal =
fn (T 0) => "CONST"
  | (T 1) => "VAR"
  | (T 2) => "ASSIGN"
  | (T 3) => "PLUS"
  | (T 4) => "MINUS"
  | (T 5) => "MUL"
  | (T 6) => "DIV"
  | (T 7) => "EOF"
  | (T 8) => "NEWLINE"
  | (T 9) => "OPBRAC"
  | (T 10) => "CLBRAC"
  | (T 11) => "PRINT"
  | (T 12) => "FOR"
  | (T 13) => "DO"
  | (T 14) => "DONE"
  | (T 15) => "TO"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.STMTS STMTS, STMTS1left, STMTS1right)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 55.33 "tiger.grm"*) STMTS (*#line 212.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, STMTS1left, STMTS1right), rest671)
end
|  ( 1, ( ( _, ( _, _, NEWLINE1right)) :: ( _, ( MlyValue.STMTS STMTS, STMTS1left, _)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 56.27 "tiger.grm"*) STMTS (*#line 216.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, STMTS1left, NEWLINE1right), rest671)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.STMTS ((*#line 58.31 "tiger.grm"*) []                       (*#line 220.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( MlyValue.STMTS STMTS, _, STMTS1right)) :: _ :: ( _, ( MlyValue.STMT STMT, STMT1left, _)) :: rest671)) => let val  result = MlyValue.STMTS ((*#line 59.28 "tiger.grm"*) STMT :: STMTS            (*#line 224.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, STMT1left, STMTS1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.VAR VAR, VAR1left, _)) :: rest671)) => let val  result = MlyValue.STMT ((*#line 61.31 "tiger.grm"*)   TIGER.Assign(VAR,EXP)   (*#line 228.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, VAR1left, EXP1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: ( _, ( _, PRINT1left, _)) :: rest671)) => let val  result = MlyValue.STMT ((*#line 62.32 "tiger.grm"*) TIGER.Print(EXP)         (*#line 232.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, PRINT1left, EXP1right), rest671)
end
|  ( 6, ( ( _, ( _, _, DONE1right)) :: ( _, ( MlyValue.STMTS STMTS, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.CONST CONST2, _, _)) :: _ :: ( _, ( MlyValue.CONST CONST1, _, _)) :: _ :: ( _, ( MlyValue.VAR VAR, _, _)) :: ( _, ( _, FOR1left, _)) :: rest671)) => let val  result = MlyValue.STMT ((*#line 63.71 "tiger.grm"*)TIGER.For(VAR,CONST1,CONST2,STMTS)(*#line 236.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, FOR1left, DONE1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.CONST CONST, CONST1left, CONST1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 65.29 "tiger.grm"*) TIGER.Val(CONST)           (*#line 240.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, CONST1left, CONST1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.VAR VAR, VAR1left, VAR1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 66.29 "tiger.grm"*) TIGER.Var(VAR)             (*#line 244.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, VAR1left, VAR1right), rest671)
end
|  ( 9, ( ( _, ( _, _, CLBRAC1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, OPBRAC1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 67.29 "tiger.grm"*) EXP (*#line 248.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, OPBRAC1left, CLBRAC1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 68.29 "tiger.grm"*) TIGER.Exp(EXP1,TIGER.Plus,EXP2)    (*#line 252.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 69.26 "tiger.grm"*) TIGER.Exp(EXP1,TIGER.Minus,EXP2)   (*#line 256.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 70.26 "tiger.grm"*) TIGER.Exp(EXP1,TIGER.Mul,EXP2)     (*#line 260.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 71.29 "tiger.grm"*) TIGER.Exp(EXP1,TIGER.Div,EXP2)     (*#line 264.1 "tiger.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.PROGRAM x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.CONST i,p1,p2))
fun VAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.VAR i,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun NEWLINE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun OPBRAC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun CLBRAC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun DONE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
end
end
