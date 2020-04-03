(*Problem №1*)
(*Task A*)
fun same_string (s1 : string, s2 : string) =
    s1 = s2
	     
fun all_except_option (str, list) =
    let 
	fun aux (str, list) = 
	    case list of
		[] => []
	      | x::xs' => if same_string(str, x)
			  then aux(str, xs')
			  else x::aux(str, xs')
    in
	if list = aux(str,list)
	then NONE
	else SOME(aux(str, list))
    end				 


	
	
(*Task B*)
fun get_substitutions1 (list_of_lists, str) =
    case list_of_lists of 
       	[] => []
      | x::xs'  => case all_except_option(str, x) of
		       NONE => get_substitutions1(xs', str)
		     | SOME value => value @ get_substitutions1(xs', str)

							       
							       
(*Task C*)								       
fun get_substitutions2 (list_of_lists, str) =
    let
	fun aux (list_of_lists, str, acc) =
	    case list_of_lists of
		[] => acc
	      | x::xs'  =>  case all_except_option(str, x) of
				NONE => aux(xs', str, acc)
			      | SOME value =>  aux(xs', str, acc @ value)
    in
	aux (list_of_lists, str, [])
    end


(*Task D*)					    
fun similar_names (list_of_lists, {first=firstName, last=lastName, middle=middleName}) =
    let
	fun add (list, middleName, lastName, acc) =
	    case list of
		[] => acc
	      | x::xs'  => add(xs', middleName, lastName, acc @ [{first=x, last=lastName, middle=middleName}])
	val sub = get_substitutions1(list_of_lists, firstName)
    in
	add (sub, middleName, lastName, [{first=firstName, last=lastName, middle=middleName}])
    end



(*Problem №2*)
datatype suit = Clubs | Diamonds | Hearts | Spades  (*Хреста, Буба, Черва, Піка*)
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(*Task A*)
fun card_color (card) =
    case card of
	(Clubs,_) => Black
      | (Spades,_) => Black
      | _  => Red


(*Task B*)
fun card_value (card) =
    case card of
	(_,Num x) => x 
      | (_,Ace) => 11
      | _ => 10
		 

(*Task C*)
fun remove_card (cs, c, e) =
    let
	fun delete_card (cs, c) =
	    case cs of
		[] => []
	     |  x::xs' => if x = c
			  then xs'
			  else x::delete_card(xs',c)    
	val held_cards = delete_card(cs, c)
    in
	if held_cards = cs
	then raise e
	else held_cards
    end
	


(*Task D*)
fun all_same_color (cards_list) =
    case cards_list of
	[] => true
	   | x::[] => true 
	   | head::neck::rest => card_color(head) = card_color(neck)
				 andalso all_same_color(neck::rest)




(*Task E*)
fun sum_cards (cards_list) =
    let
	fun aux (cards_list, acc) =
	    case cards_list of
		[] => acc
	     |  x::xs' => aux(xs', acc + card_value(x)) 
    in
	aux(cards_list, 0)
    end
	
 
	
	
	
					
					
