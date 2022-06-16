structure AST =
struct 
(* datatype  dtype = INT1 | BOOL1
datatype  boolop = TRUE1 | FALSE1 | NOT1 | AND1 | OR1
datatype  relop = LESSTHAN1 | LESSTHANEQUALS1 | GREATERTHAN1 | GREATERTHANEQUALS1 | EQUALS1 | NOTEQUALS1
datatype  addop = PLUS1 | MINUS1
datatype  multop =  MOD1 | TIMES1 | DIV1
datatype  cmds = READ1 |WRITE1 | ITE1 *)

datatype AST = AST of AST 
            | LESSTHAN of AST*AST
            | LESSTHANEQUALS of AST*AST
            | GREATERTHAN of AST*AST
            | GREATERTHANEQUALS of AST*AST
            | EQUALS of AST*AST
            | NOTEQUALS of AST*AST
            | OR of AST*AST
            | AND of AST*AST
            | NOT of AST
            | PLUS of AST*AST
            | MINUS of AST * AST
            | MOD of AST * AST
            | DIV of AST*AST
            | TIMES of AST*AST
            | TELDA of AST
            | START of string*AST
            | READ of string
            | WRITE of AST
            | ITE of AST * AST * AST
            | WH of AST * AST
            | PROG of string * ((string list * string) list * AST )
            | IDENTIFIER of string
            | BOOL of bool
            | INT of int
            | SEQ of AST list
            | SEQ2 of AST list
            | listtoAST of AST list
            | LESSTHANobj
            | LESSTHANEQUALSobj 
            | GREATERTHANobj 
            | GREATERTHANEQUALSobj 
            | EQUALSobj 
            | NOTEQUALSobj 
            | ORobj 
            | ANDobj 
            | NOTobj 
            | PLUSobj 
            | MINUSobj 
            | MODobj 
            | DIVobj 
            | TIMESobj
            | TELDAobj 
            | STARTobj
            | READobj 
            | WRITEobj 
            | ITEobj  
            | WHobj  
            | PROGobj
            | STR of string
            | ENDIF  
            | ENDWHILE
            
            
val  symboltable: (string*string*int) list ref = ref [] ;

fun list_add([],_)= ()   
    | list_add(hd::tl,dtype) = 
        let 
            val length = List.length(!symboltable)
            val _ = (symboltable := (hd,dtype,length)::(!symboltable))
        in 
  		    list_add(tl,dtype)
  	    end;
fun list_find(varname) =
	let val st = !symboltable
        fun list_find_help((var,dtype,i)::tl) = if var=varname then SOME (dtype ,i)else list_find_help(tl)
		| list_find_help ([]) = NONE
	in
		list_find_help(st)
	end;
fun list_check ([]) = NONE
    |list_check (x::xs) = 
	let val v = list_find(x) 
	in 
		case v of SOME t => SOME x | NONE => list_check (xs)
	end ;
    
fun throwerror(error) = 
	let val i = print(error)
	in 
		OS.Process.exit(OS.Process.success)
	end 

(* 
datatype  programme =PROG of PROGRAM*identifier*DOUBLECOLON*Block 
and Block = block1 of DeclarationSeq*CommandSeq | block2 of CommandSeq
and DeclarationSeq = declarationseq1 of Declaration | declarationseq2 of Declaration*DeclarationSeq
and Declaration = declaration1 of VAR*VariableList*COLON*dtype*SEMICOLON 
and VariableList = VList1 of Variable | VList2 of CommaVar*Variable
and CommaVar = CommaVar1 of COMMA*Variable | CommaVar2 of COMMA*Variable*CommaVar
and CommandSeq = CmdSeq of CURLYLEFTBRACKET*CommandSemi*CURLYRIGHTBRACKET
and CommandSemi = CommandSemi1 of Command*SEMICOLON | CommandSemi2 of Command*SEMICOLON*CommandSemi
and Command = cmd1 of VAR*START*Expression | cmd2 of READ*Variable | cmd3 of WRITE*IntExp | 
              cmd4 of IF*BoolExp*THEN*CommandSeq*ELSE*CommandSeq*ENDIF | 
              cmd5 of WHILE*BoolExp*DO*CommandSeq*ENDWH
and Expression = exp1 of IntExp | exp2 of BoolExp
and IntExp = IntExp1 of IntExp*PLUS*IntTerm | IntExp2 of IntTerm
and IntTerm = IntTerm1 of IntTerm*TIMES*IntFactor | IntTerm2 of IntFactor
and IntFactor = IntFactor1 of Numeral | IntFactor2 of Variable| IntFactor3 of LEFTBRACKET*IntExp*RIGHTBRACKET | 
                IntFactor4 of TELDA*IntFactor
and BoolExp = boolexp1 of BoolExp*OR*BoolTerm | boolexp2 of BoolTerm
and BoolTerm = boolterm1 of BoolTerm*AND*BoolFactor | boolterm2 of BoolFactor
and BoolFactor = bool1 of TRUE | bool2 of FALSE | bool3 of Variable | bool4 of Comparison | 
                 bool5 of LEFTBRACKET*BoolExp*RIGHTBRACKET | bool6 of NOT*BoolFactor
and Comparison = comparison1 of IntExp*relop*IntExp
and Variable = variable1 of identifier 
and Numeral = num of int
and DOUBLECOLON = DOUBLECOLON of string
and identifier =  id of string
and PROGRAM =  prog of string
and SEMICOLON = SEMICOLON1 of string
and COLON = colon1 of string
and VAR = var1 of string
and COMMA = comma1 of string 
and CURLYLEFTBRACKET = cbracket1 of string
and CURLYRIGHTBRACKET = cbracket2 of string 
and ENDWH = endwhile of string
and DO = do1 of string
and WHILE = whileloop of string
and ENDIF = end1 of string
and ELSE = else1 of string
and THEN = then1 of string
and IF = if1  of string
and WRITE = write1 of string
and READ = read1 of string
and START = start1 of string
and PLUS = plus1 of string
and TIMES = times1 of string
and TELDA = telda1 of string
and RIGHTBRACKET = bracket1 of string
and LEFTBRACKET = bracket2 of string
and OR = or1 of string
and AND = and1 of string
and NOT = not1 of string
and FALSE = false1 of string
and TRUE = true1 of string *)


end ;