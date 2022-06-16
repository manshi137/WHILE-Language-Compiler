# WHILE Programming Language EBNF
EBNF used is as defined in hypernotes 

# How to Run Code
    To export path in mac, enter this command in terminal before running sml files:
    export PATH="$PATH:/usr/local/smlnj/bin/"  
    The command (written in VMCStacks.sml file) used to compile the While program is:
    val x = While.compile("test.txt");
    Here "test.txt" is the name of the file with the While Program
    
    Write the While program in a file named "test.txt" 
    Keep this file in the folder containing the submitted files
    Open this folder in terminal
    Enter the following command in command to execute the program:
        sml VMCstacks.sml;
    
## Grammar implemented
1. Program ::= “program” Identifier “::”Block .
2. Block ::= DeclarationSeq CommandSeq .
3. DeclarationSeq ::= {Declaration} .
4. Declaration::= “var” VariableList“:”Type“;” .
5. Type::= “int” | “bool” .
6. VariableList::= Variable {“,” Variable} .
7. CommandSeq::= “{”{Command“;”}“}” 
8. Command::= Variable“:=”Expression |
    “read” Variable |
    “write” Expression |
    “if” Expression “then” CommandSeq “else” CommandSeq
    “endif” |
    “while” Expression “do” CommandSeq “endwh”
9. Expression ::= Expression "or" Term | Term .
10. Term ::= Term "and" Factor | Factor
11. Factor ::= Comparison | "not" Factor
12. IntFactor ::= "(”IntExpression“)” 
    | “˜”IntFactor .
    | "tt" 
    | "ff"
    | Numeral 
    | Variable 
13. Comparison ::= IntExpression AddOp IntExpression .
   | IntTerm
14. IntTerm ::= IntTerm DivOp IntFactor
   | IntFactor
15. Numeral := [+,~] NUM
16. Variable := Identifier
17. Identifier := ID

## DESIGN DECISIONS
            postfix = fn : AST -> AST list      
            postfix form is stored as a list
## AST datatype definition
##### these datatypes are used as constructors and then used in making postfix
            datatype AST =
            | AST of AST 
            | LESSTHAN of AST * AST
            | LESSTHANEQUALS of AST * AST
            | GREATERTHAN of AST * AST
            | GREATERTHANEQUALS of AST * AST
            | EQUALS of AST * AST
            | NOTEQUALS of AST * AST
            | OR of  AST * AST
            | AND of  AST * AST
            | NOT of AST
            | PLUS of AST * AST
            | MINUS of AST * AST
            | MOD of AST * AST
            | DIV of  AST * AST
            | TIMES of  AST * AST
            | TELDA of AST
            | START of string * AST
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
##### these datatypes are pushed as identifiers in postfix (list)
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
 