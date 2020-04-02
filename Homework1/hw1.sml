fun is_older (firstDate : int * int * int, secondDate: int * int * int) =
         if #1 firstDate < #1 secondDate
	 then true
         else if #2 firstDate < #2 secondDate andalso #1 firstDate = #1 secondDate
	 then true
	 else if #3 firstDate < #3 secondDate andalso #1 firstDate = #1 secondDate andalso #2 firstDate = #2 secondDate	      
	 then true
	 else false

fun number_in_month (dates : (int * int * int) list, month: int) =
	if null dates
	then 0
	else if #2 (hd dates) = month
	then number_in_month(tl dates, month) + 1
	else number_in_month(tl dates, month)

fun number_in_months (dates : (int * int * int) list, months: int list) =
   	if null months
	then 0
	else number_in_month(dates, hd months) + number_in_months(dates, tl months)									

fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then hd dates :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)
    
fun get_nth (list_of_string : string list, n : int) =
    if null list_of_string
    then ""
    else if n = 1
    then hd list_of_string
    else get_nth(tl list_of_string, n - 1)

fun date_to_string (date : (int * int * int)) =
    let
	val Name_of_months =
	    ["January", "February", "March", "April", "May", "June", "July",
	     "August", "September", "October", "November", "December"]
    in
	get_nth(Name_of_months, #2 date)^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum (sum : int, numbers : int list) =
	if sum - hd numbers > 0
	then number_before_reaching_sum(sum - hd numbers, tl numbers) + 1
	else 0

fun what_month (day_of_year : int) =
    let
	val number_of_days_in_months =
	    [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	if day_of_year > 365 orelse day_of_year < 1
	then ~1
	else number_before_reaching_sum(day_of_year, number_of_days_in_months) + 1
    end

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else let
	fun oldest_nonempty (dates : (int * int * int) list) =
	    if null (tl dates)
	    then hd dates
	    else
		 let
		    val current_oldest = oldest_nonempty(tl dates)
		 in
		     if is_older(hd dates, current_oldest)
		     then hd dates
		     else current_oldest
		 end
    in
	SOME (oldest_nonempty dates)
    end
	     
(*function for challenge problem*)
fun check (months : int list, num : int) =
    if null months
    then false
    else if hd months = num
    then true
    else check(tl months, num)
fun delete_duplicates (months : int list) =
    if null months
    then []
    else if check(tl months, hd months)
    then delete_duplicates(tl months)
    else hd months :: delete_duplicates(tl months)

fun number_in_months_challenge (dates: (int * int * int) list,  months : int list) =
	number_in_months(dates, delete_duplicates(months : int list))

fun dates_in_months_challenge (dates: (int * int * int) list,  months : int list) =
	dates_in_months(dates, delete_duplicates(months : int list))
 

fun reasonable_date (date : int * int * int) =
    let
	val number_of_days_in_months_in_leap_years =
	    [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	val number_of_days_in_months_in_non_leap_years =
	    [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	(*return number of days in particular month*)
	fun number_of_days_in_month (month : int, list_of_days: int list) =
	    if month = 1
	    then hd list_of_days
	    else number_of_days_in_month(month - 1, tl list_of_days)
	(*check for leap year*)
	fun is_leap_year (year : int) =      
	    if (year mod 400 = 0 orelse year mod 4 = 0) andalso year mod 100 <> 0
	    then true
	    else false
	(*check for month between 1 and 12*)
	fun check_month (month : int) =  
	    if month > 0 andalso month < 13
	    then true
	    else false
	(*check for appropriate day for the month depending from leap year*)
	fun day_appropriate_for_the_month (date : int * int * int) =
	    if is_leap_year(#1 date)
	    then
		 if #3 date <= number_of_days_in_month(#2 date, number_of_days_in_months_in_leap_years)
		 then true
		 else false
	    else
		if #3 date <= number_of_days_in_month(#2 date, number_of_days_in_months_in_non_leap_years)
		then true
		else false	     
    in
	if #1 date > 0 andalso check_month(#2 date) andalso day_appropriate_for_the_month(date)
        then true
	else false 
    end
	

	
	     
			 
				
		
	
	


		       
		      
		      





		  
