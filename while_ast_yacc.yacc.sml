functor WhileLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : While_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* (user definitions) *)
open AST


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\005\000\000\000\
\\001\000\001\000\005\000\002\000\044\000\024\000\043\000\026\000\042\000\
\\027\000\041\000\029\000\040\000\030\000\039\000\037\000\038\000\000\000\
\\001\000\001\000\005\000\002\000\044\000\024\000\043\000\026\000\042\000\
\\029\000\040\000\030\000\039\000\037\000\038\000\000\000\
\\001\000\001\000\005\000\012\000\023\000\013\000\022\000\014\000\021\000\
\\018\000\020\000\000\000\
\\001\000\002\000\067\000\000\000\
\\001\000\003\000\003\000\000\000\
\\001\000\005\000\050\000\006\000\049\000\000\000\
\\001\000\008\000\013\000\000\000\
\\001\000\009\000\028\000\000\000\
\\001\000\010\000\027\000\000\000\
\\001\000\010\000\072\000\000\000\
\\001\000\010\000\073\000\000\000\
\\001\000\011\000\026\000\000\000\
\\001\000\015\000\071\000\020\000\065\000\000\000\
\\001\000\016\000\091\000\000\000\
\\001\000\017\000\093\000\000\000\
\\001\000\019\000\066\000\020\000\065\000\000\000\
\\001\000\020\000\065\000\025\000\088\000\000\000\
\\001\000\021\000\025\000\000\000\
\\001\000\022\000\006\000\000\000\
\\001\000\023\000\090\000\000\000\
\\001\000\042\000\000\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\004\000\010\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\007\000\024\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\001\000\005\000\012\000\023\000\013\000\022\000\014\000\021\000\
\\018\000\020\000\000\000\
\\105\000\000\000\
\\106\000\020\000\065\000\000\000\
\\107\000\000\000\
\\108\000\020\000\065\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\028\000\056\000\000\000\
\\112\000\028\000\056\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\\118\000\000\000\
\\119\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\\123\000\037\000\058\000\038\000\057\000\000\000\
\\124\000\037\000\058\000\038\000\057\000\000\000\
\\125\000\037\000\058\000\038\000\057\000\000\000\
\\126\000\037\000\058\000\038\000\057\000\000\000\
\\127\000\037\000\058\000\038\000\057\000\000\000\
\\128\000\037\000\058\000\038\000\057\000\000\000\
\\129\000\031\000\064\000\032\000\063\000\033\000\062\000\034\000\061\000\
\\035\000\060\000\036\000\059\000\037\000\058\000\038\000\057\000\000\000\
\\130\000\039\000\055\000\040\000\054\000\041\000\053\000\000\000\
\\131\000\039\000\055\000\040\000\054\000\041\000\053\000\000\000\
\\132\000\039\000\055\000\040\000\054\000\041\000\053\000\000\000\
\\133\000\000\000\
\\134\000\000\000\
\\135\000\000\000\
\\136\000\000\000\
\\137\000\000\000\
\\138\000\000\000\
\\139\000\000\000\
\\140\000\000\000\
\"
val actionRowNumbers =
"\005\000\000\000\019\000\067\000\
\\025\000\025\000\007\000\022\000\
\\000\000\024\000\023\000\003\000\
\\028\000\018\000\066\000\012\000\
\\009\000\008\000\001\000\001\000\
\\001\000\000\000\000\000\006\000\
\\001\000\031\000\030\000\048\000\
\\049\000\042\000\063\000\041\000\
\\059\000\039\000\056\000\016\000\
\\004\000\046\000\047\000\001\000\
\\002\000\001\000\065\000\013\000\
\\035\000\034\000\029\000\010\000\
\\011\000\033\000\032\000\002\000\
\\002\000\002\000\001\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\002\000\002\000\001\000\
\\007\000\064\000\043\000\045\000\
\\017\000\007\000\027\000\026\000\
\\062\000\061\000\060\000\040\000\
\\058\000\057\000\055\000\053\000\
\\051\000\054\000\052\000\050\000\
\\038\000\020\000\044\000\014\000\
\\037\000\007\000\015\000\036\000\
\\021\000"
val gotoT =
"\
\\001\000\092\000\000\000\
\\002\000\002\000\000\000\
\\000\000\
\\000\000\
\\003\000\007\000\004\000\006\000\005\000\005\000\000\000\
\\004\000\009\000\005\000\005\000\000\000\
\\007\000\010\000\000\000\
\\000\000\
\\002\000\014\000\006\000\013\000\017\000\012\000\000\000\
\\000\000\
\\000\000\
\\002\000\014\000\008\000\017\000\009\000\016\000\017\000\015\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\014\000\010\000\035\000\011\000\034\000\012\000\033\000\
\\013\000\032\000\014\000\031\000\015\000\030\000\016\000\029\000\
\\017\000\028\000\018\000\027\000\000\000\
\\002\000\014\000\010\000\043\000\011\000\034\000\012\000\033\000\
\\013\000\032\000\014\000\031\000\015\000\030\000\016\000\029\000\
\\017\000\028\000\018\000\027\000\000\000\
\\002\000\014\000\010\000\044\000\011\000\034\000\012\000\033\000\
\\013\000\032\000\014\000\031\000\015\000\030\000\016\000\029\000\
\\017\000\028\000\018\000\027\000\000\000\
\\002\000\014\000\017\000\045\000\000\000\
\\002\000\014\000\006\000\046\000\017\000\012\000\000\000\
\\000\000\
\\002\000\014\000\010\000\049\000\011\000\034\000\012\000\033\000\
\\013\000\032\000\014\000\031\000\015\000\030\000\016\000\029\000\
\\017\000\028\000\018\000\027\000\000\000\
\\002\000\014\000\008\000\050\000\009\000\016\000\017\000\015\000\000\000\
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
\\000\000\
\\002\000\014\000\011\000\034\000\013\000\032\000\014\000\066\000\
\\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\000\000\
\\002\000\014\000\015\000\067\000\017\000\028\000\018\000\027\000\000\000\
\\002\000\014\000\010\000\068\000\011\000\034\000\012\000\033\000\
\\013\000\032\000\014\000\031\000\015\000\030\000\016\000\029\000\
\\017\000\028\000\018\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\014\000\015\000\072\000\017\000\028\000\018\000\027\000\000\000\
\\002\000\014\000\015\000\073\000\017\000\028\000\018\000\027\000\000\000\
\\002\000\014\000\015\000\074\000\017\000\028\000\018\000\027\000\000\000\
\\002\000\014\000\011\000\034\000\013\000\032\000\014\000\075\000\
\\015\000\030\000\016\000\029\000\017\000\028\000\018\000\027\000\000\000\
\\002\000\014\000\013\000\076\000\015\000\030\000\017\000\028\000\
\\018\000\027\000\000\000\
\\002\000\014\000\013\000\077\000\015\000\030\000\017\000\028\000\
\\018\000\027\000\000\000\
\\002\000\014\000\011\000\078\000\013\000\032\000\015\000\030\000\
\\017\000\028\000\018\000\027\000\000\000\
\\002\000\014\000\011\000\079\000\013\000\032\000\015\000\030\000\
\\017\000\028\000\018\000\027\000\000\000\
\\002\000\014\000\011\000\080\000\013\000\032\000\015\000\030\000\
\\017\000\028\000\018\000\027\000\000\000\
\\002\000\014\000\011\000\081\000\013\000\032\000\015\000\030\000\
\\017\000\028\000\018\000\027\000\000\000\
\\002\000\014\000\011\000\082\000\013\000\032\000\015\000\030\000\
\\017\000\028\000\018\000\027\000\000\000\
\\002\000\014\000\011\000\083\000\013\000\032\000\015\000\030\000\
\\017\000\028\000\018\000\027\000\000\000\
\\002\000\014\000\011\000\034\000\012\000\084\000\013\000\032\000\
\\014\000\031\000\015\000\030\000\016\000\029\000\017\000\028\000\
\\018\000\027\000\000\000\
\\007\000\085\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\087\000\000\000\
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
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\090\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 93
val numrules = 46
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
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUM of unit ->  (int) | ID of unit ->  (string)
 | Numeral of unit ->  (int) | Variable of unit ->  (string)
 | Comparison of unit ->  (AST*string)
 | IntFactor of unit ->  (AST*string)
 | Factor of unit ->  (AST*string) | IntTerm of unit ->  (AST*string)
 | Term of unit ->  (AST*string)
 | IntExpression of unit ->  (AST*string)
 | Expression of unit ->  (AST*string) | Command of unit ->  (AST)
 | CmdSemi of unit ->  (AST list) | CommandSeq of unit ->  (AST)
 | VariableList of unit ->  (string list)
 | Declaration of unit ->  ( ( string list * string ) )
 | DeclarationSeq of unit ->  ( ( string list * string )  list)
 | Block of unit ->  ( ( string list * string )  list*AST)
 | Identifier of unit ->  (string) | Program of unit ->  (AST)
end
type svalue = MlyValue.svalue
type result = AST
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 41) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "NUM"
  | (T 2) => "PROGRAM"
  | (T 3) => "VAR"
  | (T 4) => "INT"
  | (T 5) => "BOOL"
  | (T 6) => "COMMA"
  | (T 7) => "CURLYLEFTBRACKET"
  | (T 8) => "CURLYRIGHTBRACKET"
  | (T 9) => "SEMICOLON"
  | (T 10) => "START"
  | (T 11) => "READ"
  | (T 12) => "WRITE"
  | (T 13) => "IF"
  | (T 14) => "THEN"
  | (T 15) => "ELSE"
  | (T 16) => "ENDIF"
  | (T 17) => "WHILE"
  | (T 18) => "DO"
  | (T 19) => "OR"
  | (T 20) => "COLON"
  | (T 21) => "DOUBLECOLON"
  | (T 22) => "ENDWH"
  | (T 23) => "LEFTBRACKET"
  | (T 24) => "RIGHTBRACKET"
  | (T 25) => "TELDA"
  | (T 26) => "NOT"
  | (T 27) => "AND"
  | (T 28) => "FALSE"
  | (T 29) => "TRUE"
  | (T 30) => "LESSTHAN"
  | (T 31) => "GREATERTHAN"
  | (T 32) => "EQUALS"
  | (T 33) => "LESSTHANEQUALS"
  | (T 34) => "GREATERTHANEQUALS"
  | (T 35) => "NOTEQUALS"
  | (T 36) => "PLUS"
  | (T 37) => "MINUS"
  | (T 38) => "MOD"
  | (T 39) => "TIMES"
  | (T 40) => "DIV"
  | (T 41) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35)
 $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28)
 $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21)
 $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14)
 $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7)
 $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Block Block1, _, Block1right)) :: _ :: ( _,
 ( MlyValue.Identifier Identifier1, _, _)) :: ( _, ( _, PROGRAM1left,
 _)) :: rest671)) => let val  result = MlyValue.Program (fn _ => let
 val  (Identifier as Identifier1) = Identifier1 ()
 val  (Block as Block1) = Block1 ()
 in (PROG(Identifier,Block))
end)
 in ( LrTable.NT 0, ( result, PROGRAM1left, Block1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.CommandSeq CommandSeq1, _, CommandSeq1right)
) :: ( _, ( MlyValue.DeclarationSeq DeclarationSeq1, 
DeclarationSeq1left, _)) :: rest671)) => let val  result = 
MlyValue.Block (fn _ => let val  (DeclarationSeq as DeclarationSeq1) =
 DeclarationSeq1 ()
 val  (CommandSeq as CommandSeq1) = CommandSeq1 ()
 in (DeclarationSeq , CommandSeq)
end)
 in ( LrTable.NT 2, ( result, DeclarationSeq1left, CommandSeq1right), 
rest671)
end
|  ( 2, ( ( _, ( MlyValue.DeclarationSeq DeclarationSeq1, _, 
DeclarationSeq1right)) :: ( _, ( MlyValue.Declaration Declaration1, 
Declaration1left, _)) :: rest671)) => let val  result = 
MlyValue.DeclarationSeq (fn _ => let val  (Declaration as Declaration1
) = Declaration1 ()
 val  (DeclarationSeq as DeclarationSeq1) = DeclarationSeq1 ()
 in (Declaration::DeclarationSeq)
end)
 in ( LrTable.NT 3, ( result, Declaration1left, DeclarationSeq1right),
 rest671)
end
|  ( 3, ( rest671)) => let val  result = MlyValue.DeclarationSeq (fn _
 => ([]))
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( _, _, SEMICOLON1right)) :: _ :: _ :: ( _, ( 
MlyValue.VariableList VariableList1, _, _)) :: ( _, ( _, VAR1left, _))
 :: rest671)) => let val  result = MlyValue.Declaration (fn _ => let
 val  (VariableList as VariableList1) = VariableList1 ()
 in (
(case list_check(VariableList) of SOME alpha => throwerror("\nERROR: variable declaration already exists \n") | NONE => list_add(VariableList,"int")); (VariableList, "int") 
)
end)
 in ( LrTable.NT 4, ( result, VAR1left, SEMICOLON1right), rest671)
end
|  ( 5, ( ( _, ( _, _, SEMICOLON1right)) :: _ :: _ :: ( _, ( 
MlyValue.VariableList VariableList1, _, _)) :: ( _, ( _, VAR1left, _))
 :: rest671)) => let val  result = MlyValue.Declaration (fn _ => let
 val  (VariableList as VariableList1) = VariableList1 ()
 in (
(case list_check(VariableList) of SOME alpha => throwerror("\nERROR: variable declaration already exists \n") | NONE => list_add(VariableList,"bool")); (VariableList, "bool") 
)
end)
 in ( LrTable.NT 4, ( result, VAR1left, SEMICOLON1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.Variable Variable1, Variable1left, 
Variable1right)) :: rest671)) => let val  result = 
MlyValue.VariableList (fn _ => let val  (Variable as Variable1) = 
Variable1 ()
 in ([Variable])
end)
 in ( LrTable.NT 5, ( result, Variable1left, Variable1right), rest671)

end
|  ( 7, ( ( _, ( MlyValue.VariableList VariableList1, _, 
VariableList1right)) :: _ :: ( _, ( MlyValue.Variable Variable1, 
Variable1left, _)) :: rest671)) => let val  result = 
MlyValue.VariableList (fn _ => let val  (Variable as Variable1) = 
Variable1 ()
 val  (VariableList as VariableList1) = VariableList1 ()
 in (Variable::VariableList)
end)
 in ( LrTable.NT 5, ( result, Variable1left, VariableList1right), 
rest671)
end
|  ( 8, ( ( _, ( _, _, CURLYRIGHTBRACKET1right)) :: ( _, ( 
MlyValue.CmdSemi CmdSemi1, _, _)) :: ( _, ( _, CURLYLEFTBRACKET1left,
 _)) :: rest671)) => let val  result = MlyValue.CommandSeq (fn _ =>
 let val  (CmdSemi as CmdSemi1) = CmdSemi1 ()
 in (SEQ(CmdSemi))
end)
 in ( LrTable.NT 6, ( result, CURLYLEFTBRACKET1left, 
CURLYRIGHTBRACKET1right), rest671)
end
|  ( 9, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.Command 
Command1, Command1left, _)) :: rest671)) => let val  result = 
MlyValue.CmdSemi (fn _ => let val  (Command as Command1) = Command1 ()
 in ([Command])
end)
 in ( LrTable.NT 7, ( result, Command1left, SEMICOLON1right), rest671)

end
|  ( 10, ( ( _, ( MlyValue.CmdSemi CmdSemi1, _, CmdSemi1right)) :: _
 :: ( _, ( MlyValue.Command Command1, Command1left, _)) :: rest671))
 => let val  result = MlyValue.CmdSemi (fn _ => let val  (Command as 
Command1) = Command1 ()
 val  (CmdSemi as CmdSemi1) = CmdSemi1 ()
 in (Command::CmdSemi)
end)
 in ( LrTable.NT 7, ( result, Command1left, CmdSemi1right), rest671)

end
|  ( 11, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right
)) :: _ :: ( _, ( MlyValue.Variable Variable1, Variable1left, _)) :: 
rest671)) => let val  result = MlyValue.Command (fn _ => let val  (
Variable as Variable1) = Variable1 ()
 val  (Expression as Expression1) = Expression1 ()
 in (
case list_find(Variable) of SOME alpha => (if ((#2 Expression) = (#1 alpha)) then (START(Variable, (#1 Expression))) else throwerror("\n ERROR: variable and expression types do not agree \n")) | NONE => throwerror("\n ERROR: unknown variable \n")
)
end)
 in ( LrTable.NT 8, ( result, Variable1left, Expression1right), 
rest671)
end
|  ( 12, ( ( _, ( MlyValue.Variable Variable1, _, Variable1right)) :: 
( _, ( _, READ1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (Variable as Variable1) = Variable1
 ()
 in (READ( Variable))
end)
 in ( LrTable.NT 8, ( result, READ1left, Variable1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right
)) :: ( _, ( _, WRITE1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 in (WRITE(#1 Expression))
end)
 in ( LrTable.NT 8, ( result, WRITE1left, Expression1right), rest671)

end
|  ( 14, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.CommandSeq 
CommandSeq2, _, _)) :: _ :: ( _, ( MlyValue.CommandSeq CommandSeq1, _,
 _)) :: _ :: ( _, ( MlyValue.Expression Expression1, _, _)) :: ( _, (
 _, IF1left, _)) :: rest671)) => let val  result = MlyValue.Command
 (fn _ => let val  (Expression as Expression1) = Expression1 ()
 val  CommandSeq1 = CommandSeq1 ()
 val  CommandSeq2 = CommandSeq2 ()
 in (
if((#2 Expression)="bool") then ITE(#1 Expression, CommandSeq1, CommandSeq2 ) else throwerror("\n ERROR: condition for if should be boolean expression\n")
)
end)
 in ( LrTable.NT 8, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 15, ( ( _, ( _, _, ENDWH1right)) :: ( _, ( MlyValue.CommandSeq 
CommandSeq1, _, _)) :: _ :: ( _, ( MlyValue.Expression Expression1, _,
 _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (Expression as Expression1) = 
Expression1 ()
 val  (CommandSeq as CommandSeq1) = CommandSeq1 ()
 in (
if((#2 Expression)="bool") then WH(#1 Expression, CommandSeq) else throwerror("\n ERROR: condition for do-while should be boolean expression\n" )
)
end)
 in ( LrTable.NT 8, ( result, WHILE1left, ENDWH1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.Term Term1, _, Term1right)) :: _ :: ( _, ( 
MlyValue.Expression Expression1, Expression1left, _)) :: rest671)) =>
 let val  result = MlyValue.Expression (fn _ => let val  (Expression
 as Expression1) = Expression1 ()
 val  (Term as Term1) = Term1 ()
 in (
if(((#2 Expression) = "bool") andalso ((#2 Term) = "bool")) then (OR((#1 Expression),(#1 Term)),"bool") else throwerror("\n ERROR: erroneous operand type \n")
)
end)
 in ( LrTable.NT 9, ( result, Expression1left, Term1right), rest671)

end
|  ( 17, ( ( _, ( MlyValue.Term Term1, Term1left, Term1right)) :: 
rest671)) => let val  result = MlyValue.Expression (fn _ => let val  (
Term as Term1) = Term1 ()
 in (Term)
end)
 in ( LrTable.NT 9, ( result, Term1left, Term1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.Factor Factor1, _, Factor1right)) :: _ :: (
 _, ( MlyValue.Term Term1, Term1left, _)) :: rest671)) => let val  
result = MlyValue.Term (fn _ => let val  (Term as Term1) = Term1 ()
 val  (Factor as Factor1) = Factor1 ()
 in (
if(((#2 Term) = "bool") andalso ((#2 Factor) = "bool")) then (AND((#1 Term), (#1 Factor)),"bool") else throwerror("\n ERROR: erroneous operand type\n")
)
end)
 in ( LrTable.NT 11, ( result, Term1left, Factor1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.Factor Factor1, Factor1left, Factor1right))
 :: rest671)) => let val  result = MlyValue.Term (fn _ => let val  (
Factor as Factor1) = Factor1 ()
 in (Factor)
end)
 in ( LrTable.NT 11, ( result, Factor1left, Factor1right), rest671)

end
|  ( 20, ( ( _, ( MlyValue.Comparison Comparison1, Comparison1left, 
Comparison1right)) :: rest671)) => let val  result = MlyValue.Factor
 (fn _ => let val  (Comparison as Comparison1) = Comparison1 ()
 in (Comparison)
end)
 in ( LrTable.NT 13, ( result, Comparison1left, Comparison1right), 
rest671)
end
|  ( 21, ( ( _, ( MlyValue.Factor Factor1, _, Factor1right)) :: ( _, (
 _, NOT1left, _)) :: rest671)) => let val  result = MlyValue.Factor
 (fn _ => let val  (Factor as Factor1) = Factor1 ()
 in (
if((#2 Factor) = "bool") then (NOT((#1 Factor)),"bool") else throwerror("\n ERROR: erroneous operand type  \n")
)
end)
 in ( LrTable.NT 13, ( result, NOT1left, Factor1right), rest671)
end
|  ( 22, ( ( _, ( _, _, RIGHTBRACKET1right)) :: ( _, ( 
MlyValue.Expression Expression1, _, _)) :: ( _, ( _, LEFTBRACKET1left,
 _)) :: rest671)) => let val  result = MlyValue.IntFactor (fn _ => let
 val  (Expression as Expression1) = Expression1 ()
 in (Expression)
end)
 in ( LrTable.NT 14, ( result, LEFTBRACKET1left, RIGHTBRACKET1right), 
rest671)
end
|  ( 23, ( ( _, ( MlyValue.IntFactor IntFactor1, _, IntFactor1right))
 :: ( _, ( _, TELDA1left, _)) :: rest671)) => let val  result = 
MlyValue.IntFactor (fn _ => let val  (IntFactor as IntFactor1) = 
IntFactor1 ()
 in (
if((#2 IntFactor)="int") then (TELDA((#1 IntFactor)),"int") else throwerror("\n ERROR: erroneous operand type \n")
)
end)
 in ( LrTable.NT 14, ( result, TELDA1left, IntFactor1right), rest671)

end
|  ( 24, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.IntFactor (fn _ => (BOOL(true),"bool"))
 in ( LrTable.NT 14, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 25, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.IntFactor (fn _ => (BOOL(false),"bool"))
 in ( LrTable.NT 14, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.Numeral Numeral1, Numeral1left, 
Numeral1right)) :: rest671)) => let val  result = MlyValue.IntFactor
 (fn _ => let val  (Numeral as Numeral1) = Numeral1 ()
 in ((INT(Numeral),"int"))
end)
 in ( LrTable.NT 14, ( result, Numeral1left, Numeral1right), rest671)

end
|  ( 27, ( ( _, ( MlyValue.Variable Variable1, Variable1left, 
Variable1right)) :: rest671)) => let val  result = MlyValue.IntFactor
 (fn _ => let val  (Variable as Variable1) = Variable1 ()
 in (
case list_find(Variable) of SOME alpha => (IDENTIFIER(Variable),(#1 alpha)) | NONE => throwerror("\n ERROR: unknown variable  \n")
)
end)
 in ( LrTable.NT 14, ( result, Variable1left, Variable1right), rest671
)
end
|  ( 28, ( ( _, ( MlyValue.IntExpression IntExpression2, _, 
IntExpression2right)) :: _ :: ( _, ( MlyValue.IntExpression 
IntExpression1, IntExpression1left, _)) :: rest671)) => let val  
result = MlyValue.Comparison (fn _ => let val  IntExpression1 = 
IntExpression1 ()
 val  IntExpression2 = IntExpression2 ()
 in (
if((#2 IntExpression1)= (#2 IntExpression2 )) then (LESSTHAN((#1 IntExpression1), (#1 IntExpression2)),"bool") else throwerror ("\n ERROR: erroneous operand type \n")
)
end)
 in ( LrTable.NT 15, ( result, IntExpression1left, IntExpression2right
), rest671)
end
|  ( 29, ( ( _, ( MlyValue.IntExpression IntExpression2, _, 
IntExpression2right)) :: _ :: ( _, ( MlyValue.IntExpression 
IntExpression1, IntExpression1left, _)) :: rest671)) => let val  
result = MlyValue.Comparison (fn _ => let val  IntExpression1 = 
IntExpression1 ()
 val  IntExpression2 = IntExpression2 ()
 in (
if((#2 IntExpression1)= (#2 IntExpression2 )) then (LESSTHANEQUALS((#1 IntExpression1), (#1 IntExpression2)),"bool") else throwerror ("\n ERROR: erroneous operand type \n")
)
end)
 in ( LrTable.NT 15, ( result, IntExpression1left, IntExpression2right
), rest671)
end
|  ( 30, ( ( _, ( MlyValue.IntExpression IntExpression2, _, 
IntExpression2right)) :: _ :: ( _, ( MlyValue.IntExpression 
IntExpression1, IntExpression1left, _)) :: rest671)) => let val  
result = MlyValue.Comparison (fn _ => let val  IntExpression1 = 
IntExpression1 ()
 val  IntExpression2 = IntExpression2 ()
 in (
if((#2 IntExpression1)= (#2 IntExpression2 )) then (GREATERTHAN((#1 IntExpression1), (#1 IntExpression2)),"bool") else throwerror ("\n ERROR: erroneous operand type \n")
)
end)
 in ( LrTable.NT 15, ( result, IntExpression1left, IntExpression2right
), rest671)
end
|  ( 31, ( ( _, ( MlyValue.IntExpression IntExpression2, _, 
IntExpression2right)) :: _ :: ( _, ( MlyValue.IntExpression 
IntExpression1, IntExpression1left, _)) :: rest671)) => let val  
result = MlyValue.Comparison (fn _ => let val  IntExpression1 = 
IntExpression1 ()
 val  IntExpression2 = IntExpression2 ()
 in (
if((#2 IntExpression1)= (#2 IntExpression2 )) then (GREATERTHANEQUALS((#1 IntExpression1), (#1 IntExpression2)),"bool") else throwerror ("\n ERROR: erroneous operand type \n")
)
end)
 in ( LrTable.NT 15, ( result, IntExpression1left, IntExpression2right
), rest671)
end
|  ( 32, ( ( _, ( MlyValue.IntExpression IntExpression2, _, 
IntExpression2right)) :: _ :: ( _, ( MlyValue.IntExpression 
IntExpression1, IntExpression1left, _)) :: rest671)) => let val  
result = MlyValue.Comparison (fn _ => let val  IntExpression1 = 
IntExpression1 ()
 val  IntExpression2 = IntExpression2 ()
 in (
if((#2 IntExpression1)= (#2 IntExpression2 )) then (EQUALS((#1 IntExpression1), (#1 IntExpression2)),"bool") else throwerror ("\n ERROR: erroneous operand type \n")
)
end)
 in ( LrTable.NT 15, ( result, IntExpression1left, IntExpression2right
), rest671)
end
|  ( 33, ( ( _, ( MlyValue.IntExpression IntExpression2, _, 
IntExpression2right)) :: _ :: ( _, ( MlyValue.IntExpression 
IntExpression1, IntExpression1left, _)) :: rest671)) => let val  
result = MlyValue.Comparison (fn _ => let val  IntExpression1 = 
IntExpression1 ()
 val  IntExpression2 = IntExpression2 ()
 in (
if((#2 IntExpression1)= (#2 IntExpression2 )) then (NOTEQUALS((#1 IntExpression1), (#1 IntExpression2)),"bool") else throwerror ("\n ERROR: erroneous operand type \n")
)
end)
 in ( LrTable.NT 15, ( result, IntExpression1left, IntExpression2right
), rest671)
end
|  ( 34, ( ( _, ( MlyValue.IntExpression IntExpression1, 
IntExpression1left, IntExpression1right)) :: rest671)) => let val  
result = MlyValue.Comparison (fn _ => let val  (IntExpression as 
IntExpression1) = IntExpression1 ()
 in (IntExpression)
end)
 in ( LrTable.NT 15, ( result, IntExpression1left, IntExpression1right
), rest671)
end
|  ( 35, ( ( _, ( MlyValue.IntTerm IntTerm1, _, IntTerm1right)) :: _
 :: ( _, ( MlyValue.IntExpression IntExpression1, IntExpression1left,
 _)) :: rest671)) => let val  result = MlyValue.IntExpression (fn _ =>
 let val  (IntExpression as IntExpression1) = IntExpression1 ()
 val  (IntTerm as IntTerm1) = IntTerm1 ()
 in (
if(((#2 IntExpression) = "int") andalso ((#2 IntTerm) = "int")) then (PLUS((#1 IntExpression),(#1 IntTerm)),"int") else throwerror("\nERROR: erroneous operand type \n")
)
end)
 in ( LrTable.NT 10, ( result, IntExpression1left, IntTerm1right), 
rest671)
end
|  ( 36, ( ( _, ( MlyValue.IntTerm IntTerm1, _, IntTerm1right)) :: _
 :: ( _, ( MlyValue.IntExpression IntExpression1, IntExpression1left,
 _)) :: rest671)) => let val  result = MlyValue.IntExpression (fn _ =>
 let val  (IntExpression as IntExpression1) = IntExpression1 ()
 val  (IntTerm as IntTerm1) = IntTerm1 ()
 in (
if(((#2 IntExpression) = "int") andalso ((#2 IntTerm) = "int")) then (MINUS((#1 IntExpression),(#1 IntTerm)),"int") else throwerror("\nERROR: erroneous operand type \n")
)
end)
 in ( LrTable.NT 10, ( result, IntExpression1left, IntTerm1right), 
rest671)
end
|  ( 37, ( ( _, ( MlyValue.IntTerm IntTerm1, IntTerm1left, 
IntTerm1right)) :: rest671)) => let val  result = 
MlyValue.IntExpression (fn _ => let val  (IntTerm as IntTerm1) = 
IntTerm1 ()
 in (IntTerm)
end)
 in ( LrTable.NT 10, ( result, IntTerm1left, IntTerm1right), rest671)

end
|  ( 38, ( ( _, ( MlyValue.IntFactor IntFactor1, _, IntFactor1right))
 :: _ :: ( _, ( MlyValue.IntTerm IntTerm1, IntTerm1left, _)) :: 
rest671)) => let val  result = MlyValue.IntTerm (fn _ => let val  (
IntTerm as IntTerm1) = IntTerm1 ()
 val  (IntFactor as IntFactor1) = IntFactor1 ()
 in (
if ((#2 IntTerm ) = "int") andalso ((#2 IntFactor)= "int") then (MOD((#1 IntTerm) , (#1 IntFactor)), "int") else  throwerror("\n ERROR: erroneous operand type \n")
)
end)
 in ( LrTable.NT 12, ( result, IntTerm1left, IntFactor1right), rest671
)
end
|  ( 39, ( ( _, ( MlyValue.IntFactor IntFactor1, _, IntFactor1right))
 :: _ :: ( _, ( MlyValue.IntTerm IntTerm1, IntTerm1left, _)) :: 
rest671)) => let val  result = MlyValue.IntTerm (fn _ => let val  (
IntTerm as IntTerm1) = IntTerm1 ()
 val  (IntFactor as IntFactor1) = IntFactor1 ()
 in (
if ((#2 IntTerm ) = "int") andalso ((#2 IntFactor)= "int") then (TIMES((#1 IntTerm) , (#1 IntFactor)), "int") else  throwerror("\n ERROR: erroneous operand type \n")
)
end)
 in ( LrTable.NT 12, ( result, IntTerm1left, IntFactor1right), rest671
)
end
|  ( 40, ( ( _, ( MlyValue.IntFactor IntFactor1, _, IntFactor1right))
 :: _ :: ( _, ( MlyValue.IntTerm IntTerm1, IntTerm1left, _)) :: 
rest671)) => let val  result = MlyValue.IntTerm (fn _ => let val  (
IntTerm as IntTerm1) = IntTerm1 ()
 val  (IntFactor as IntFactor1) = IntFactor1 ()
 in (
if ((#2 IntTerm ) = "int") andalso ((#2 IntFactor)= "int") then (DIV((#1 IntTerm) , (#1 IntFactor)), "int") else  throwerror("\n ERROR: erroneous operand type \n")
)
end)
 in ( LrTable.NT 12, ( result, IntTerm1left, IntFactor1right), rest671
)
end
|  ( 41, ( ( _, ( MlyValue.IntFactor IntFactor1, IntFactor1left, 
IntFactor1right)) :: rest671)) => let val  result = MlyValue.IntTerm
 (fn _ => let val  (IntFactor as IntFactor1) = IntFactor1 ()
 in (IntFactor)
end)
 in ( LrTable.NT 12, ( result, IntFactor1left, IntFactor1right), 
rest671)
end
|  ( 42, ( ( _, ( MlyValue.NUM NUM1, _, NUM1right)) :: ( _, ( _, 
PLUS1left, _)) :: rest671)) => let val  result = MlyValue.Numeral (fn
 _ => let val  (NUM as NUM1) = NUM1 ()
 in (NUM)
end)
 in ( LrTable.NT 17, ( result, PLUS1left, NUM1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.Numeral (fn _ => let val  (NUM as NUM1
) = NUM1 ()
 in (NUM)
end)
 in ( LrTable.NT 17, ( result, NUM1left, NUM1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.Identifier Identifier1, Identifier1left, 
Identifier1right)) :: rest671)) => let val  result = MlyValue.Variable
 (fn _ => let val  (Identifier as Identifier1) = Identifier1 ()
 in (Identifier)
end)
 in ( LrTable.NT 16, ( result, Identifier1left, Identifier1right), 
rest671)
end
|  ( 45, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.Identifier (fn _ => let val  (ID as ID1) =
 ID1 ()
 in (ID)
end)
 in ( LrTable.NT 1, ( result, ID1left, ID1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : While_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun PROGRAM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun CURLYLEFTBRACKET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun CURLYRIGHTBRACKET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun START (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun DOUBLECOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun LEFTBRACKET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun RIGHTBRACKET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun TELDA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSTHANEQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHANEQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun NOTEQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
end
end
