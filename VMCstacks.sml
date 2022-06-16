Control.Print.printLength := 1000;
Control.Print.printDepth := 1000;
Control.Print.stringDepth := 1000;
CM.make "WHILE.cm";

(* use "stack.sml";
use "compiler.sml"; *)

(* use "while_ast.sml"; *)

open AST;
use "stack.sml";
(* open Stack; *)

val stackC= FunStack.create;
val stackV=  FunStack.create;

(* memory stores (varname, value) tuple *)
val memory: ((string*int) array) ref = ref (Array.array (100,("",0))) ;


fun getString(arr) = let fun its ((x,y),b) = b ^ Int.toString((y)) ^ ", " in "{ " ^ ((Array.foldl its) "" arr) ^ "}" end 

(* fun write_in_memory(var,value)= 
(case AST.list_find(var) of SOME (a,b) => 
     Array.update(!memory,b,(var,value))   
    | NONE => AST.throwerror("\n ERROR: unknown variable \n")); *)

fun find_in_memory(varname) =
	(case AST.list_find(varname) of SOME (a,b) => Array.sub(!memory, b) | NONE => ("",0));



(* pushing in the stack(implemented as list) in postfix manner 
the expressions/commands/variables along with operators of type AST *)

fun postfix(INT(a)) = [INT(a)]
    | postfix(BOOL(a)) = [BOOL(a)]
    | postfix(IDENTIFIER(a)) = [IDENTIFIER(a)]
    | postfix(PLUS(a,b)) = postfix(b)@postfix(a)@[(PLUSobj)]
    | postfix(MINUS(a,b)) = postfix(b)@postfix(a)@[(MINUSobj)]
    | postfix(MOD(a,b)) = postfix(b)@postfix(a)@[(MODobj)]
    | postfix(TIMES(a,b))= postfix(b)@postfix(a)@[(TIMESobj)]
    | postfix(DIV(a,b)) = postfix(b)@postfix(a)@[(DIVobj)]
    | postfix(LESSTHAN(a,b)) = postfix(b)@postfix(a)@[(LESSTHANobj)]
    | postfix(LESSTHANEQUALS(a,b)) = postfix(b)@postfix(a)@[(LESSTHANEQUALSobj)]
    | postfix(GREATERTHAN(a,b)) = postfix(b)@postfix(a)@[(GREATERTHANobj)]
    | postfix(GREATERTHANEQUALS(a,b) )= postfix(b)@postfix(a)@[(GREATERTHANEQUALSobj)]
    | postfix(EQUALS(a,b)) = postfix(b)@postfix(a)@[(EQUALSobj)]
    | postfix(NOTEQUALS(a,b)) = postfix(b)@postfix(a)@[(NOTEQUALSobj)]
    | postfix(OR(a,b)) = postfix(b)@postfix(a)@[(ORobj)]
    | postfix(AND(a,b)) = postfix(b)@postfix(a)@[(ANDobj)]
    | postfix(NOT(a)) = postfix(a)@[(NOTobj)]
    | postfix(TELDA(a)) = postfix(a)@[(TELDAobj)]
    | postfix(READ(a)) = [STR(a)]@[(READobj)]
    | postfix(WRITE(a)) = postfix(a)@[((WRITEobj))]
    | postfix(ITE(a,b,c)) = [(ENDIF)]@postfix(c)@postfix(b)@postfix(a)@[(ITEobj)]
    | postfix(WH(a,b)) = let val x::xs = postfix(b) val SEQ2(y) = x in [ENDIF]@[ENDWHILE]@[SEQ2(y @ [WH(a,b)])]@postfix(a) @ [ITEobj] end
    | postfix(START(a,b)) = [STR(a)]@postfix(b)@[(STARTobj)]
    | postfix(SEQ(a)) = let val b=postfix(listtoAST(a)) in [SEQ2(b)] end
    | postfix(listtoAST([])) =  [] 
    | postfix(listtoAST(x::xs)) = postfix(x)@postfix(listtoAST(xs)) 
    (* | postfix(PROG(a,(b,c))) = postfix(c) *)
    | postfix(_) = []

(* ========================================== *)
(* | postfix(SEQ(x::xs)) = postfix(x)@postfix(SEQ(xs)) 
| postfix(SEQ([])) = [SEQ()] *)

(* takes the postfix order list and updates the stacks according to the commands *)
(* ??? how are the commands pushed in cstack *)

(* fun rules (v,a::(b::(WHobj::c))) =  *)

fun rules((INT(a)::(INT(b)::v)), ((PLUSobj):: c)) = rules( (INT(a+b)::v),c)

    | rules( (INT(a)::(INT(b)::v)), ((MINUSobj)::c) ) =
        let 
        val v1 = (INT(a-b)::v)
        in 
        rules(v1,c)
        end
    | rules( (INT(a)::(INT(b):: v)), ((MODobj)::c)) =
        let 
        val v1 = (INT(a mod b)::v)
        in 
        rules(v1,c)
        end

    | rules( (INT(a)::(INT(b):: v)), ((DIVobj)::c)) =
        let 
        val v1 = (INT(a div b)::v)
        in 
        rules(v1,c)
        end

        (* ============================================ *)
    | rules( (INT(a)::(INT(b):: v)), ((TIMESobj)::c)) =
        let 
        val v1 = (INT(a * b)::v)
        in 
        rules(v1,c)
        end
    | rules( (INT(a)::(INT(b):: v)), ((LESSTHANobj)::c)) =
        let 
        val bool = if a<b then 1 else 0;
        val v1 = INT(bool)::v
        in 
        rules(v1,c)
        end
    | rules( (INT(a)::(INT(b):: v)), (LESSTHANEQUALSobj)::c ) =
        let 
        val bool = if a<=b then 1 else 0;
        val v1 = INT(bool)::v
        in 
        rules(v1,c)
        end
    | rules((INT(a)::(INT(b):: v)), (GREATERTHANobj)::c ) =
        let 
        val bool = if a>b then 1 else 0;
        val v1 = INT(bool)::v
        in 
        rules(v1,c)
        end
    | rules( (INT(a)::(INT(b):: v)), (GREATERTHANEQUALSobj)::c ) =
        let 
        val bool = if a>=b then 1 else 0;
        val v1 = INT(bool)::v
        in 
        rules(v1,c)
        end
    | rules( (INT(a)::(INT(b):: v)), (EQUALSobj)::c ) =
        let 
        val bool = if a=b then 1 else 0;
        val v1 = INT(bool)::v
        in 
        rules(v1,c)
        end
    | rules( (INT(a)::(INT(b):: v)), (NOTEQUALSobj)::c ) =
        let 
        val bool = if a=b then 0 else 1;
        val v1 = INT(bool)::v
        in 
        rules(v1,c)
        end
    | rules( (INT(a)::(INT(b):: v)), (ORobj)::c ) =
        let 
        val bool = if( a=0 andalso b=0) then 0 else 1;
        val v1 = INT(bool)::v
        in 
        rules(v1,c)
        end
    | rules((INT(a)::(INT(b):: v)), (ANDobj)::c ) =
        let 
        val bool = if( a=1 andalso b=1) then 1 else 0;
        val v1 = INT(bool)::v
        in 
        rules(v1,c)
        end
    | rules( (INT(a):: v), (NOTobj)::c ) =
        let 
        val bool = if( a=1 ) then 0 else 1;
        val v1 = INT(bool)::v
        in 
        rules(v1,c)
        end

    | rules( (INT(a):: v), (TELDAobj)::c ) =
        let 
        val bool = (~1)*a
        val v1 = (INT(bool)::v)
        in 
        rules(v1,c)
        end

    | rules (v, (INT(a)::c)) = 
            rules(INT(a)::v, c)
    | rules (v, BOOL(a)::c) = 
        let val bool = if(a= false) then 0 else 1
        in rules(INT(bool)::v,c)
        end
    | rules (v, IDENTIFIER(a)::c) = 
        let val (s,x) = find_in_memory(a)
        in rules(INT(x)::v,c)
        end
    | rules(INT(b)::(STR(a)::v), STARTobj::c) = 
        let
            fun write_in_memory(var,value)= 
(case AST.list_find(var) of SOME (a,b) => 
     Array.update(!memory,b,(var,value))   
    | NONE => AST.throwerror("\n ERROR: unknown variable \n"));
         val _ = write_in_memory(a,b) 
        in rules(v, c)
        end
    | rules(INT(a)::v, WRITEobj::c) = 
        let val _ = print(Int.toString(a))
        in rules(v,c)
        end 
    (* | rules(STR(a)::v, READobj::c) = 
        let val value = valOf(Int.fromString(valOf(TextIO.inputLine(TextIO.stdIn))))
        val add = write_in_memory(a, value)
        in rules(v, c)
        end *)
    | rules(v, SEQ2(a)::c) = 
        let val lambda = rules([],a)
        in rules(v,c)
        end 
    | rules(v,(ENDIF::(a::(b::c)))) = 
        let 
        val v1= b::(a::v)
        in rules(v1,c)
        end
    | rules(v,STR(a)::c) = rules(STR(a)::v, c)
    | rules (INT(a)::(SEQ2(seqb)::(t::v)), ITEobj::c) = 
        if(a=1) then rules(v,SEQ2(seqb)::c) 
        else (case t of SEQ2(seqc) => rules(v,SEQ2(seqc)::c) | _ => rules(v,c))
    | rules (v,WH(a,b)::c) = let val x = postfix(WH(a,b)) val _ = rules([],x) in rules(v,c) end
    | rules(_,[]) = getString(!memory) (* print memory*)
    | rules(_,_) = "" (* error *)


val index: int ref = ref 0;
fun increment (x) = x:= !x + 1;
fun initialize_memory(IDENTIFIER(a)) = let val c= increment(index) in
Array.update(!memory,!index,(a,0)) end
    | initialize_memory(listtoAST(x::xs)) = initialize_memory(listtoAST(xs))
    | initialize_memory(_) = ();

fun execute(v,c) = let val m = rules(v,c) 
val vstk = v
val cstk = c
in (vstk,memory,cstk) end;

val PROG(a,(b,x)) = While.compile("test.txt");
print("\n calling postfix function \n");
val y = postfix(x);

val mmm = initialize_memory(x);

print("\n calling rules function \n");
val z= rules([],y);
print("\n calling execute function \n");
val beta = AST.symboltable;

val exe= execute([],y);
print("\n");
val a: unit = OS.Process.exit(OS.Process.success);
