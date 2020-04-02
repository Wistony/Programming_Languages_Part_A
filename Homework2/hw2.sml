(*Problem №1*)
(*First task*)
fun same_string (s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (str, list) =
    case list of
	[] => []
      | hd::tl => if same_string(str, hd)
		  then all_except_option(str, tl)
		  else hd::all_except_option(str, tl)



(*Second task*)
fun get_substitutions1 (list_of_lists, str) =
    case list_of_lists of 
	[] => []
      | hd::tl  => if all_except_option(str, hd) = hd
		   then get_substitutions1(tl, str)
		   else all_except_option(str,hd) @ get_substitutions1(tl, str)			   

		      

					    
