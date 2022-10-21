fun alternate (nums : int list) =
    let 
      fun partial_sum (sign : bool, nums : int list) = 
          if null nums then 0
          else if sign then (hd nums) + partial_sum (not sign, tl nums)
          else 0 - (hd nums) + partial_sum (not sign, tl nums)
    in
      partial_sum (true, nums)
    end

fun min_max (nums : int list) = 
    let 
      fun min(nums : int list) = 
          if null nums then 2000000000
          else
            let val tl_ans = min (tl nums)
            in
              if (hd nums) < tl_ans then hd nums else tl_ans
            end
      fun max(nums: int list) = 
          if null nums then 0
          else
            let val tl_ans = max (tl nums)
            in
              if (hd nums) > tl_ans then hd nums else tl_ans
            end
    in
      (min(nums), max(nums))
    end


fun cumsum (nums : int list) = 
    if null nums then []
    else
      let fun helper (curr_sum : int, nums : int list) =
            if null nums then []
            else curr_sum + (hd nums) :: helper (curr_sum + (hd nums), tl nums)
      in
        helper (0, nums)
      end

fun greeting (name : string option) =
    if isSome name
    then "Hello there, " ^ (valOf name) ^ "!"
    else "Hello there, you!"

fun repeat (nums : int list, counter : int list) =
    if null nums orelse null counter then []
    else
      let fun helper (num : int, count : int) = 
              if count > 1 then num :: helper (num, count - 1)
              else [num]
          val tail_ans = repeat (tl nums, tl counter)
      in
          if (hd counter) > 0 then helper (hd nums, hd counter) @ tail_ans
          else tail_ans
      end

fun addOpt (num1 : int option, num2 : int option) = 
    if isSome num1 andalso isSome num2 then SOME ((valOf num1) + (valOf num2))
    else NONE

fun addAllOpt (nums : int option list) =
    if null nums then NONE
    else
      let val tl_ans = addAllOpt (tl nums)
      in
        if isSome tl_ans andalso isSome (hd nums) then SOME ((valOf (hd nums)) + (valOf tl_ans))
        else if isSome (hd nums) then SOME (valOf (hd nums))
        else if isSome tl_ans then SOME (valOf tl_ans)
        else NONE
      end

fun any (entries : bool list) = 
    if null entries then false
    else (hd entries) orelse any (tl entries)

fun all (entries : bool list) =
    if null entries then true
    else (hd entries) andalso all (tl entries)

fun zip (nums1 : int list, nums2 : int list) = 
    if null nums1 orelse null nums2 then []
    else
        (hd nums1, hd nums2) :: zip (tl nums1, tl nums2)

fun zipRecycle (nums1 : int list, nums2 : int list) =
    let fun helper (list1 : int list, list2 : int list) =
            if null list1 andalso null list2 then []
            else if null list1 andalso null (tl list2) then [(hd nums1) :: (hd list2)] 
            else if null list1 then zipRecycle (nums1, list2)
            else if null list2 andalso null (tl list1) then [(hd list1) :: (hd nums2)]
            else if null list2 then zipRecycle (list1, nums2)
            else (hd list1, hd list2) :: helper (tl list1, tl list2)
    in
        helper (nums1, nums2)
    end

fun zipOpt (nums1 : int list, nums2 : int list) =
    let fun len (nums1 : int list) = 
            if null nums1 then 0
            else 1 + len (tl nums1)

    

fun lookup (pairs : (string*int) list, str : string) = 
    if null pairs then NONE
    else if (#1 (hd pairs)) = str then SOME (#2 (hd pairs))
    else lookup (tl pairs, str)

fun isSorted (nums : int list) = 
    if null nums then true
    else 
      let val tail = tl nums
      in
        if null tail then true
        else (hd nums) < (hd tail) andalso isSorted(tail)
      end

fun isSortedDecreasing (nums : int list) = 
    if null nums then true
    else
      let val tail = tl nums
      in
        if null tail then true
        else (hd nums) > (hd tail) andalso isSortedDecreasing(tail)
      end

fun isAnySorted (nums : int list) =
    isSorted (nums) orelse isSortedDecreasing (nums)









fun splitup (nums : int list) = 
    let fun helper (pos : int list, neg : int list, nums : int list) =
            if null nums then (pos, neg)
            else if (hd nums) >= 0 then helper (pos @ [(hd nums)], neg, tl nums)
            else helper (pos, neg @ [(hd nums)], tl nums)
    in
        helper ([], [], nums)
    end

fun splitAt (nums : int list, th : int) =
    let fun helper (pos : int list, neg : int list, nums : int list) = 
            if null nums then (pos, neg)
            else if (hd nums) >= th then helper (pos @ [(hd nums)], neg, tl nums)
            else helper (pos, neg @ [(hd nums)], tl nums)
    in
        helper ([], [], nums)
    end

fun sortedMerge (list1 : int list, list2 : int list) = 
    let fun helper (list1 : int list, list2 : int list, res : int list) = 
            if null list1 andalso null list2 then res
            else if null list1 then helper (list1, tl list2, res @ [(hd list2)])
            else if null list2 then helper (tl list1, list2, res @ [(hd list1)])
            else if (hd list1) <= (hd list2) then helper (tl list1, list2, res @ [(hd list1)])
            else helper (list1, tl list2, res @ [(hd list2)])
    in
        helper (list1, list2, [])
    end
