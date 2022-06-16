signature VMC =
    sig
    type 'a vmc 
    val create: -> 'a vmc
    val E_m: 'a  FunStack * 'a  FunStack -> 'a  FunStack
    val E_x: 'a  FunStack * 'a  FunStack -> 'a  FunStack
    val E_00: 'a  FunStack * 'a  FunStack -> 'a  FunStack
    val E_01: 'a  FunStack * 'a  FunStack -> 'a  FunStack
    val E_10: 'a  FunStack * 'a  FunStack -> 'a  FunStack
    val E_11: 'a  FunStack * 'a  FunStack -> 'a  FunStack
    val E: 'a  FunStack * 'a  FunStack -> 'a  FunStack
    val C00: 'a  FunStack * 'a  FunStack -> 'a  FunStack
    val C0: 'a  FunStack * 'a  FunStack -> 'a  FunStack
    val C1: 'a  FunStack * 'a  FunStack -> 'a  FunStack
    val C: 'a  FunStack * 'a  FunStack -> 'a  FunStack
    val C_ite: 'a  FunStack * 'a  FunStack -> 'a  FunStack
    val C_ite0: 'a  FunStack * 'a  FunStack -> 'a  FunStack
    val C_ite1: 'a  FunStack * 'a  FunStack -> 'a  FunStack
    val C_wh: 'a  FunStack * 'a  FunStack -> 'a  FunStack
    val C_wh0: 'a  FunStack * 'a  FunStack -> 'a  FunStack
    val C_wh1: 'a  FunStack * 'a  FunStack -> 'a  FunStack
    
(* (case list_check(VariableList) of SOME alpha => throwerror("\nERROR: variable declaration already exists \n") | NONE => list_add(VariableList,"int"); ) *)

    end
structure Vmc :> VMC = 
    struct 
    type 'a vmc = 'a stack * 'a list * 'a stack
    val create = ([],memory,[])9
    fun E_m(vstack, cstack) =    let val m= FunStack.top(cstack) val y= FunStack.pop(cstack) in  FunStack.push(m,vstack) end

    fun E_x(vstack, cstack) =    let val x= FunStack.top(cstack) val y= FunStack.pop(cstack) in (case find_in_memory(x) of SOME a =>  FunStack.push(a,vstack) 
                                | NONE => throwerror("\n ERROR: variable not found \n "); )end

    fun E_00(vstack, cstack) =    let val m= FunStack.top(cstack) val y= FunStack.pop(cstack) val n=  FunStack.top(cstack) val z= FunStack.pop(cstack) 
                        val q=  FunStack.push(m,vstack) in  FunStack.push(n,vstack) end

    fun E_01(vstack, cstack) =    let val m= FunStack.top(cstack) val mm= FunStack.pop(cstack) val x=  FunStack.top(cstack) val z= FunStack.pop(cstack) 
                        val q=  FunStack.push(m,vstack) in (case find_in_memory(x) of SOME a =>  FunStack.push(a,vstack) 
                                | NONE => throwerror("\n ERROR: variable not found \n "); ) end

    fun E_10(vstack, cstack) =    let val x= FunStack.top(cstack) val y= FunStack.pop(cstack) val m=  FunStack.top(cstack) val z= FunStack.pop(cstack) 
                        in (case find_in_memory(x) of SOME a =>  FunStack.push(a,vstack) ;  FunStack.push(m,vstack)
                                | NONE => throwerror("\n ERROR: variable not found \n "); ) end

    fun E_11(vstack, cstack) =    let val x= FunStack.top(cstack) val b= FunStack.pop(cstack) val y=  FunStack.top(cstack) val z= FunStack.pop(cstack) 
                        (* val x=  FunStack.push(!x,vstack) in  FunStack.push(!y,vstack) end *)
                        in (case find_in_memory(x) of SOME a =>  FunStack.push(a,vstack) ; 
                                | NONE => throwerror("\n ERROR: variable not found \n "); ) 
                            (case find_in_memory(y) of SOME b =>  FunStack.push(b,vstack) ; 
                                | NONE => throwerror("\n ERROR: variable not found \n "); ) end CM.make "While.cm"
    fun E(vstack, cstack) =       let val n= FunStack.top(vstack) val y= FunStack.pop(vstack) val m=  FunStack.top(vstack) val z= FunStack.pop(vstack)
                        val o= FunStack.top(cstack) val oo= FunStack.pop(cstack)  
                        val p=o(m,n)  in  FunStack.push(p,vstack) end
    fun C = let val c= FunStack.top(cstack) val cc= FunStack.pop(cstack) val d= FunStack.top(cstack) val dd= FunStack.pop(cstack) val seq= FunStack.pop(cstack)
    lat val cc= FunStack.push(c,cstack) in  FunStack.push(d,cstack) end
    fun C_ite = let val b= FunStack.top(cstack) val bb= FunStack.pop(cstack) in  FunStack.push(b,vstack) end
    fun C_ite0 = let val c= FunStack.top(cstack) val cc= FunStack.pop(cstack) val d= FunStack.top(cstack) val dd= FunStack.pop(cstack) val ite= FunStack.pop(cstack)
    in  FunStack.push(d,cstack) end
    fun C_ite1 = let val c= FunStack.top(cstack) val cc= FunStack.pop(cstack) val d= FunStack.top(cstack) val dd= FunStack.pop(cstack) val ite= FunStack.pop(cstack)
    in  FunStack.push(c,cstack) end
    fun C_wh = let val b= FunStack.top(cstack) val bb= FunStack.pop(cstack) val c= FunStack.top(cstack) val cc= FunStack.pop(cstack) 
    val bbb= FunStack.push(b,vstack) val ccc= FunStack.push(c,vstack) in  FunStack.push(b,cstack) end
    
