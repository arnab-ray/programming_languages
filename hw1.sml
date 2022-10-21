fun is_older (fd : int*int*int, sd : int*int*int) =
  if (#1 fd) < (#1 sd)
  then true
  else
    if (#1 fd) = (#1 sd)
    then
      if (#2 fd < #2 sd) then true
      else
        if (#2 fd = #2 sd) then (#3 fd) < (#3 sd)
        else false
    else
      false

fun number_in_month (dates : (int*int*int) list, month : int) = 
    if null dates
    then
        0
    else
        let val rem_dates = tl dates
        in
            if #2 (hd dates) = month
            then 1 + number_in_month (rem_dates, month)
            else 0 + number_in_month (rem_dates, month)
        end

fun number_in_months(dates : (int*int*int) list, months : int list) = 
    if null dates orelse null months
    then 0
    else number_in_month (dates, hd months) + number_in_months (dates, tl months)

fun dates_in_month (dates : (int*int*int) list, month : int) = 
    if null dates
    then []
    else
        let val rem_dates = tl dates
        in
            if #2 (hd dates) = month
            then hd dates :: dates_in_month (rem_dates, month)
            else dates_in_month (rem_dates, month)
        end

fun dates_in_months (dates : (int*int*int) list, months : int list) = 
    if null dates orelse null months
    then []
    else dates_in_month (dates, hd months) @ dates_in_months (dates, tl months)

fun get_nth (strs : string list, n : int) = 
    if n > 1 then get_nth (tl strs, n - 1)
    else hd strs

fun date_to_string (date : int*int*int) = 
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months, #2 date) ^ " " ^ Int.toString (#3 date) ^ ", " ^  Int.toString (#1 date)
    end

fun number_before_reaching_sum (sum : int, nums : int list) = 
    let fun count (counter : int, curr_sum : int, rem_nums : int list) = 
            if curr_sum + (hd rem_nums) >= sum then counter
            else count (counter + 1, curr_sum + (hd rem_nums), tl rem_nums)
    in
        count (0, 0, nums)
    end

fun what_month (day : int) = 
    let val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 30]
    in
        number_before_reaching_sum(day, days) + 1
    end

fun month_range (start_date : int, end_date : int) = 
    if start_date > end_date then []
    else what_month (start_date) :: month_range (start_date + 1, end_date)

fun oldest (dates : (int*int*int) list) = 
    if null dates
    then NONE
    else
        let fun oldest_date (dates : (int*int*int) list) = 
                if null (tl dates)
                then hd dates
                else 
                    let val tl_ans = oldest_date (tl dates)
                    in
                        if is_older (hd dates, tl_ans)
                        then hd dates
                        else tl_ans
                    end
        in
            SOME (oldest_date dates)
        end


fun remove (element : int, elements : int list) = 
    if null elements then []
    else if element = (hd elements) then remove (element, tl elements)
    else (hd elements) :: remove (element, tl elements)

fun remove_dup (elements : int list) =
    if null elements then []
    else (hd elements) :: remove_dup(remove((hd elements), (tl elements)))

fun number_in_months_challenge (dates : (int*int*int) list, months : int list) = 
    let val months = remove_dup months
    in
      number_in_months (dates, months)
    end

fun dates_in_months_challenge (dates : (int*int*int) list, months : int list) =
    let val months = remove_dup months
    in 
      dates_in_months (dates, months)
    end

fun reasonable_date(date : int*int*int) = 
    let
        val year = #1 date
        val month = #2 date
        val day = #3 date
        val leap_year = year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
        val feb_days = if leap_year then 29 else 28
        val days = [31, feb_days, 31, 30, 31, 30, 31, 31, 30, 31, 30, 30]
        fun get_nth (days : int list, n : int) =
            if n > 1 then get_nth (tl days, n - 1)
            else hd days
    in
        year > 1 andalso month >= 1 andalso month <= 12 andalso day >= 1 andalso day <= get_nth (days, month)
    end
