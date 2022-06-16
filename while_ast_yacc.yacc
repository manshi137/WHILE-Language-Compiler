(* (user definitions) *)
open AST

%%
%name While

%term  ID of string | NUM of int |  PROGRAM | VAR | INT | BOOL | COMMA | CURLYLEFTBRACKET | CURLYRIGHTBRACKET |
SEMICOLON | START | READ | WRITE | IF | THEN | ELSE | ENDIF | WHILE | DO | OR | COLON |DOUBLECOLON |
ENDWH | LEFTBRACKET | RIGHTBRACKET | TELDA | NOT | AND | FALSE | TRUE | 
LESSTHAN | GREATERTHAN | EQUALS | LESSTHANEQUALS | GREATERTHANEQUALS | NOTEQUALS | 
PLUS | MINUS | MOD | TIMES | DIV |EOF 

%nonterm Program of AST | 
Identifier of string | 
Block of (string list * string) list * AST | 
DeclarationSeq of (string list * string) list | 
Declaration of (string list * string) | 
VariableList of string list | 
CommandSeq of AST | CmdSemi of AST list | Command of AST | Expression of AST * string | IntExpression of AST * string | Term of AST * string | IntTerm of AST * string | Factor of AST * string | IntFactor of AST * string  | Comparison of AST * string | Variable of string | Numeral of int 

%pos int
%arg (fileName) : string
%eop EOF
%noshift EOF
%start Program
%verbose

%% 
Program :       PROGRAM Identifier DOUBLECOLON Block (PROG(Identifier,Block))
Block :         DeclarationSeq CommandSeq (DeclarationSeq , CommandSeq)
DeclarationSeq : Declaration DeclarationSeq (Declaration::DeclarationSeq) 
                |  ([]) 
Declaration :   VAR VariableList COLON INT SEMICOLON ((case list_check(VariableList) of SOME alpha => throwerror("\nERROR: variable declaration already exists \n") | NONE => list_add(VariableList,"int")); (VariableList, "int") )
                | VAR VariableList COLON BOOL SEMICOLON ((case list_check(VariableList) of SOME alpha => throwerror("\nERROR: variable declaration already exists \n") | NONE => list_add(VariableList,"bool")); (VariableList, "bool") )
VariableList :  Variable ([Variable]) 
                | Variable COMMA VariableList (Variable::VariableList)
CommandSeq :    CURLYLEFTBRACKET CmdSemi CURLYRIGHTBRACKET(SEQ(CmdSemi))
CmdSemi       : Command SEMICOLON ([Command])
                | Command SEMICOLON CmdSemi (Command::CmdSemi)       
Command :       Variable START Expression  (case list_find(Variable) of SOME alpha => (if ((#2 Expression) = (#1 alpha)) then (START(Variable, (#1 Expression))) else throwerror("\n ERROR: variable and expression types do not agree \n")) | NONE => throwerror("\n ERROR: unknown variable \n"))
                | READ Variable(READ( Variable)) 
                | WRITE Expression(WRITE(#1 Expression)) 
                | IF Expression THEN CommandSeq ELSE CommandSeq ENDIF (if((#2 Expression)="bool") then ITE(#1 Expression, CommandSeq1, CommandSeq2 ) else throwerror("\n ERROR: condition for if should be boolean expression\n"))
                | WHILE Expression DO CommandSeq ENDWH (if((#2 Expression)="bool") then WH(#1 Expression, CommandSeq) else throwerror("\n ERROR: condition for do-while should be boolean expression\n" ))
Expression :    Expression OR Term (if(((#2 Expression) = "bool") andalso ((#2 Term) = "bool")) then (OR((#1 Expression),(#1 Term)),"bool") else throwerror("\n ERROR: erroneous operand type \n"))
                | Term (Term)
Term :          Term AND Factor (if(((#2 Term) = "bool") andalso ((#2 Factor) = "bool")) then (AND((#1 Term), (#1 Factor)),"bool") else throwerror("\n ERROR: erroneous operand type\n"))
                | Factor (Factor)
Factor :    Comparison(Comparison)
            | NOT Factor (if((#2 Factor) = "bool") then (NOT((#1 Factor)),"bool") else throwerror("\n ERROR: erroneous operand type  \n"))

IntFactor:  LEFTBRACKET Expression RIGHTBRACKET(Expression) 
            | TELDA IntFactor (if((#2 IntFactor)="int") then (TELDA((#1 IntFactor)),"int") else throwerror("\n ERROR: erroneous operand type \n"))          
            | TRUE (BOOL(true),"bool")
            | FALSE (BOOL(false),"bool")
            | Numeral ((INT(Numeral),"int"))
            | Variable (case list_find(Variable) of SOME alpha => (IDENTIFIER(Variable),(#1 alpha)) | NONE => throwerror("\n ERROR: unknown variable  \n"))

Comparison :  IntExpression LESSTHAN IntExpression (if((#2 IntExpression1)= (#2 IntExpression2 )) then (LESSTHAN((#1 IntExpression1), (#1 IntExpression2)),"bool") else throwerror ("\n ERROR: erroneous operand type \n"))
            | IntExpression LESSTHANEQUALS IntExpression (if((#2 IntExpression1)= (#2 IntExpression2 )) then (LESSTHANEQUALS((#1 IntExpression1), (#1 IntExpression2)),"bool") else throwerror ("\n ERROR: erroneous operand type \n"))
            | IntExpression GREATERTHAN IntExpression (if((#2 IntExpression1)= (#2 IntExpression2 )) then (GREATERTHAN((#1 IntExpression1), (#1 IntExpression2)),"bool") else throwerror ("\n ERROR: erroneous operand type \n"))
            | IntExpression GREATERTHANEQUALS IntExpression (if((#2 IntExpression1)= (#2 IntExpression2 )) then (GREATERTHANEQUALS((#1 IntExpression1), (#1 IntExpression2)),"bool") else throwerror ("\n ERROR: erroneous operand type \n"))
            | IntExpression EQUALS IntExpression (if((#2 IntExpression1)= (#2 IntExpression2 )) then (EQUALS((#1 IntExpression1), (#1 IntExpression2)),"bool") else throwerror ("\n ERROR: erroneous operand type \n"))
            | IntExpression NOTEQUALS IntExpression (if((#2 IntExpression1)= (#2 IntExpression2 )) then (NOTEQUALS((#1 IntExpression1), (#1 IntExpression2)),"bool") else throwerror ("\n ERROR: erroneous operand type \n"))
            | IntExpression (IntExpression)
                
IntExpression : IntExpression PLUS IntTerm (if(((#2 IntExpression) = "int") andalso ((#2 IntTerm) = "int")) then (PLUS((#1 IntExpression),(#1 IntTerm)),"int") else throwerror("\nERROR: erroneous operand type \n"))
                |  IntExpression MINUS IntTerm (if(((#2 IntExpression) = "int") andalso ((#2 IntTerm) = "int")) then (MINUS((#1 IntExpression),(#1 IntTerm)),"int") else throwerror("\nERROR: erroneous operand type \n"))
                | IntTerm (IntTerm)

IntTerm:         IntTerm MOD IntFactor (if ((#2 IntTerm ) = "int") andalso ((#2 IntFactor)= "int") then (MOD((#1 IntTerm) , (#1 IntFactor)), "int") else  throwerror("\n ERROR: erroneous operand type \n"))
                | IntTerm TIMES IntFactor (if ((#2 IntTerm ) = "int") andalso ((#2 IntFactor)= "int") then (TIMES((#1 IntTerm) , (#1 IntFactor)), "int") else  throwerror("\n ERROR: erroneous operand type \n"))      
                | IntTerm DIV IntFactor (if ((#2 IntTerm ) = "int") andalso ((#2 IntFactor)= "int") then (DIV((#1 IntTerm) , (#1 IntFactor)), "int") else  throwerror("\n ERROR: erroneous operand type \n")) 
                | IntFactor (IntFactor)  

Numeral :   PLUS NUM (NUM)
            | NUM (NUM)
Variable :      Identifier(Identifier)
Identifier :    ID (ID)