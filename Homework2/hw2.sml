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
	
 
	
(*Task F*)
fun score (held_cards, goal) =
    let
	val sum  = sum_cards(held_cards)
    in
	case (sum > goal, all_same_color(held_cards)) of
	   (true,true) => ((sum - goal) * 3) div 2
	 | (true,false) => (sum - goal) * 3
	 | (false,true) => (goal - sum) div 2 
	 | (false,false) => goal - sum
    end
	
		 
(*Task G*)
(*cs - card_list*)
(*hc - held_card*)	
fun officiate (card_list, move_list, goal) =
    let
	fun make_move ([], cs, hc) = hc (*if move_list is empty, game is over*)
	  | make_move (mv_hd::mv_tl, cs, hc) = 
	    case (mv_hd, cs) of
		(Draw,[]) => hc (*if card_list is empty, game is over*)
	      | (Draw,x::xs') => if sum_cards(x::hc) > goal  (*check or drawing causes the sum of the held-cards to exceed the goal*)
				 then x::hc (* if exceed, make drawing and game is over*)
				 else make_move(mv_tl, xs', x::hc) (*if not exceed, play continues*)
	      | (Discard c,_) => make_move(mv_tl, cs, remove_card(hc, c, IllegalMove))
    in
	score(make_move(move_list, card_list, []), goal)
    end
	



	(*  | (mv_hd::mv_tl, cs_hd::cs_tl) => case mv_hd of
						   Draw => make_move(mv_tl, cs_tl, cs_hd::held_cards)
						 | Discard card => make_move(mv_tl, cs_hd::cs_tl,remove_card(held_cards, card, IllegalMove) handle IllegalMove => false)	*)
	
					
					
