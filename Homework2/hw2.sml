(*Problem №1*)
(*First task*)
fun same_string (s1 : string, s2 : string) =
    s1 = s2
	     
fun all_except_option (str, list) =
    let 
	fun aux (str, list) = 
	    case list of
		[] => []
	      | hd::tl => if same_string(str, hd)
			  then aux(str, tl)
			  else hd::aux(str, tl)
	val new_list = aux(str, list)
    in
	if new_list = list
	then NONE
	else SOME(new_list)
    end				 
    
(*Second task*)
fun get_substitutions1 (list_of_lists, str) =
    case list_of_lists of 
       	[] => []
      | hd::tl  => case all_except_option(str, hd) of
		       NONE => get_substitutions1(tl, str)
		     | SOME x => x @ get_substitutions1(tl, str)
(*Third task*)								       
fun get_substitutions2 (list_of_lists, str) =
    let
	fun aux (list_of_lists, str, acc) =
	    case list_of_lists of
		[] => acc
	      | hd::tl  =>  case all_except_option(str, hd) of
				NONE => aux(tl, str, acc)
			      | SOME x =>  aux(tl, str, acc @ x)
    in
	aux (list_of_lists, str, [])
    end

    
