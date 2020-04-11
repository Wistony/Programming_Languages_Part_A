(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(*****Task 1*****)
fun only_capitals str_list =
    List.filter (fn s => Char.isUpper(String.sub(s, 0))) str_list


		
(*****Task 2*****)
fun longest_string1 str_list =
     List.foldl (fn (x,y) => if String.size x > String.size y
			     then x
			     else y) "" str_list

		
(*****Task 3*****)
fun longest_string2 str_list =
     List.foldl (fn (x,y) => if String.size x >= String.size y
			     then x
			     else y) "" str_list


		
(*****Task 4*****)
fun longest_string_helper f  = 
    List.foldl (fn (x,y) => if f(String.size x, String.size y)
			    then x
			    else y) "" 
	       
val longest_string3 = longest_string_helper (fn (x,y) => x > y)
					    
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)


					    
(*****Task 5*****)				    
val longest_capitalized = longest_string1 o only_capitals


						
(*****Task 6*****)
val rev_string = String.implode o List.rev o String.explode



(*****Task 7*****)						 
fun first_answer f list =
    case list of
	[] => raise NoAnswer
      | x::xs' => case f x of
		     SOME y => y
		   | NONE => first_answer f xs'

				   
(*****Task 8*****)
fun all_answers f list =
    let
	fun aux acc list =
	    case list of
		[] => SOME acc
	      | x::xs' => case f x of
			      SOME y => aux (y@acc) xs'
			    | NONE  => NONE
    in
	aux [] list
    end



(*****Task 9*****)
(*a*)
val count_wildcards = g (fn () => 1) (fn _ => 0)

(*b*)			  
val count_wild_and_variable_lengths = g (fn () => 1) String.size

(*c*)
fun count_some_var (s,p) = g (fn () => 0 ) (fn x => if s = x then 1 else 0) p					

(*****Task 10*****)			     
fun check_pat p = 
    let
	fun loop p = (*return all variables*)
	    case p of
		Variable x => [x]
	      | TupleP ps  => List.foldl (fn (p,px) => loop p @ px) [] ps
	      | ConstructorP (_,p) => loop p
	      | _ => []
	fun have_duplicates str = (* return true if have duplicates*)
	    case str of
		[] => false
	     | x::xs' => List.exists (fn s => x = s) xs' orelse have_duplicates xs'
    in
	not (have_duplicates (loop p))
    end
	

(*****Task 11*****)
fun match (v, p) =
    case (v, p) of
        (_,Wildcard) => SOME []
     |  (Unit,UnitP) => SOME []
     |  (_,Variable x) => SOME [(x,v)]
     |  (Const x, ConstP y) => if x = y
			       then SOME []
			       else NONE			       
     |  (Tuple vs, TupleP ps) => if List.length vs = List.length ps
				 then all_answers match (ListPair.zip(vs,ps)) 
	 		         else NONE
     |  (Constructor (s1,v), ConstructorP (s2,p)) => if s1 = s2
						     then match (v,p)
						     else NONE 
     |  _ => NONE



(*****Task 12*****)
fun first_match v p =
    SOME (first_answer (fn x => match(v,x)) p)
		 handle NoAnswer => NONE





    
