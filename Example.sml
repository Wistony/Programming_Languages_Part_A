(*Records*)
val x = { one=(1,true), two=1, three=(false, 1+5)};
#one x;

(*Datatype*)
datatype mytype = TwoInts of int * int
		| Str of string
                | Pizza

(*Case of*)		      
fun f(x : mytype) =
    case x of
	Pizza => 3
     | Str s => 8 
     | TwoInts(i1,i2) => i1+i2

(*Type*)				
datatype suit = Diamond | Club | Heart | Spade
datatype rank = Ace | Jack | Queen | King | Num of int
type card = suit * rank						       
	 
	 
