structure Tokens = Tokens;

type pos = int ;
type svalue = Tokens.svalue;
type ('a,'b) token = ('a, 'b) Tokens.token;
type lexresult = (svalue, pos) token;
type lexarg = string;
type arg = lexarg;



val col =ref 1
val row =ref 1
val eof = fn (fileName) => Tokens.EOF(!row, !row)
val error = fn (e, l:int,h:int,_) => TextIO.output(TextIO.stdOut,"Unknown token :" ^ (Int.toString l)^ ": "^(Int.toString h) ^ ": " ^ e ^ "\n")
exception UnknownToken

(* "Unknown token :" ^ (Int.toString l)^ ": "^(Int.toString c) ^ ": " ^ s ^ "\n" *)

(*----------------check fn------------------ *)

%%
%full
%header (functor WhileLexFun(structure Tokens:While_TOKENS));
%arg (fileName:string);
uppercase = [A-Z];
lowercase = [a-z];
digit = [0-9];
alphabet = [a-zA-Z];
alphadigit = [0-9a-zA-Z];
space = [\ \r];


%%
\n =>           (row := !row + 1; col := (!col)-(!col);continue());
\t =>           (col := !col + 4; continue());
{space} =>      (col := !col + String.size(yytext); continue());
"program" =>    (col := !col+7; Tokens.PROGRAM(!row, !row));
"::"=>          (col := !col+ 2; Tokens.DOUBLECOLON(!row,!row));
":"=>           (col := !col+ 1; Tokens.COLON(!row,!row));
"var"=>         (col := !col +3; Tokens.VAR(!row, !row));
"int"=>         (col := !col +3; Tokens.INT(!row, !row));
"bool"=>        (col := !col +4; Tokens.BOOL(!row,!row));
","=>           (col := !col +1; Tokens.COMMA(!row,!row));
"{"=>           (col := !col +1; Tokens.CURLYLEFTBRACKET(!row,!row));
"}"=>           (col := !col +1; Tokens.CURLYRIGHTBRACKET(!row,!row));
";"=>           (col := !col +1; Tokens.SEMICOLON(!row,!row));
":="=>          (col := !col +2; Tokens.START(!row,!row));
"read"=>        (col := !col +4; Tokens.READ(!row,!row));
"write"=>       (col := !col +5; Tokens.WRITE(!row,!row));
"if"=>          (col := !col +2; Tokens.IF(!row,!row));
"then"=>        (col := !col +4; Tokens.THEN(!row,!row));
"else"=>        (col := !col +4; Tokens.ELSE(!row,!row));
"endif"=>       (col := !col +5; Tokens.ENDIF(!row,!row));
"while"=>       (col := !col +5; Tokens.WHILE(!row,!row));
"do"=>          (col := !col +2; Tokens.DO(!row,!row));
"endwh"=>       (col := !col +5; Tokens.ENDWH(!row,!row));
"("=>           (col := !col +1; Tokens.LEFTBRACKET(!row,!row));
")"=>           (col := !col +1; Tokens.RIGHTBRACKET(!row,!row));
"~"=>           (col := !col +1; Tokens.TELDA(!row,!row));
"||"=>          (col := !col +2; Tokens.OR(!row,!row));
"&&"=>          (col := !col +2; Tokens.AND(!row,!row));
"ff"=>          (col := !col +2; Tokens.FALSE(!row,!row));
"tt"=>          (col := !col +2; Tokens.TRUE(!row,!row));
"!" =>          (col := !col +1; Tokens.NOT(!row, !row));
"<"=>           (col := !col +1; Tokens.LESSTHAN(!row,!row));
">"=>           (col := !col +1; Tokens.GREATERTHAN(!row,!row));
"="=>           (col := !col +1; Tokens.EQUALS(!row,!row));
"<="=>          (col := !col +2; Tokens.LESSTHANEQUALS(!row,!row));
">="=>          (col := !col +2; Tokens.GREATERTHANEQUALS(!row,!row));
"<>"=>          (col := !col +2; Tokens.NOTEQUALS(!row,!row));
"+"=>           (col := !col +1; Tokens.PLUS(!row,!row));
"-"=>           (col := !col +1; Tokens.MINUS(!row, !row));
"%"=>           (col := !col +1; Tokens.MOD(!row,!row));
"*"=>           (col := !col +1; Tokens.TIMES(!row,!row));
"/"=>           (col := !col +1; Tokens.DIV(!row,!row));
{alphabet}{alphadigit}* =>(col := (!col) + String.size(yytext) ; Tokens.ID(yytext, !row,!row));
{digit}+ =>     (col := (!col) + String.size(yytext) ; Tokens.NUM(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext), !row, !row));