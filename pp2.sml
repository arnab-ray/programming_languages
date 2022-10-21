(* data types defined for 1 - 4*)
type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail

fun pass_or_fail final_grade =
    case final_grade of
         {id = _, grade = NONE} => fail
        | {id = _, grade = SOME i} => if i >= 75 then pass else fail

fun has_passed final_grade =
    case pass_or_fail(final_grade) of
         pass => true
       | fail => false

fun number_passed (grades : final_grade list) =
    case grades of
         [] => 0
       | x::x' => (if has_passed(x) then 1 else 0) + number_passed(x')

fun number_misgraded (pairs : (pass_fail * final_grade) list) =
    case pairs of
         [] => 0
       | x::x' => (if pass_or_fail(#2 x) <> (#1 x) then 1 else 0) + number_misgraded(x')

(* data types defined for 5 - 7*)
datatype 'a tree = leaf 
                 | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = leave_me_alone | prune_me

fun tree_height tree =
    case tree of
         leaf => 0
       | node { value = _, left = ltree, right = rtree } => 1 + Int.max(tree_height(ltree), tree_height(rtree))

fun sum_tree tree =
    case tree of
         leaf => 0
       | node { value = nval, left = ltree, right = rtree } => nval + sum_tree(ltree) + sum_tree(rtree)

fun gardener tree =
    case tree of
         node { value = leave_me_alone, left = ltree, right = rtree } => 
           node { value = leave_me_alone, left = gardener ltree, right = gardener rtree }
       | _ => leaf

(* data types defined for 9 - 16*)
datatype nat = ZERO | SUCC of nat
exception Negative

fun is_positive num =
    case num of
         ZERO => false
       | _ => true

fun pred num =
    case num of
         ZERO => raise Negative
       | SUCC x => x

fun nat_to_int num =
    case num of
         ZERO => 0
       | SUCC x => 1 + nat_to_int x

fun int_to_nat int_num =
    if int_num < 0 then raise Negative
    else if int_num = 0 then ZERO
    else SUCC (int_to_nat (int_num - 1))


fun add (first : nat, second : nat) =
    case second of
         ZERO => first
       | SUCC x => SUCC (add (first, x))

fun sub (first : nat, second : nat) =
    case (first, second) of
         (_, ZERO) => first
       | (ZERO, second) => raise Negative
       | (SUCC x, SUCC y) => sub (x, y)

fun mult (first : nat, second : nat) =
    let fun helper (snum, acc) =
        case snum of
             ZERO => ZERO
           | SUCC ZERO => add (first, acc)
           | SUCC y => helper (y, add (first, acc))
    in
      helper (second, ZERO)
    end

fun less_than (first : nat, second : nat) =
    let val fnum = nat_to_int first
        val snum = nat_to_int second
    in
      if fnum < snum then true else false
    end

(* data type for 17 - 19 *)
datatype intSet = 
  Elems of int list (*list of integers, possibly with duplicates to be ignored*)
| Range of { from : int, to : int }  (* integers from one number to another *)
| Union of intSet * intSet (* union of the two sets *)
| Intersection of intSet * intSet (* intersection of the two sets *)

fun list_contains (nums, x) =
    case nums of
         [] => false
       | s::x' => if (s = x) then true else list_contains (x', x)

fun rm_duplicates_in_list nums =
    let fun helper (num_list, acc) =
            case num_list of
                 [] => acc
               | x::x' => helper(x', if list_contains (acc, x) then acc else acc @ [x])
    in
      helper(nums, [])
    end

fun range_to_list range =
    let fun helper (r, acc) =
        case r of
             Range {from = low, to = high} => if low > high
                                     then acc 
                                     else helper (Range {from = low, to = high - 1}, high::acc)
    in
      helper (range, [])
    end

fun union_to_list (set1, set2) =
    let fun helper (set, acc) =
            case set of
                 [] => acc
               | x::x' => helper (x', if list_contains (acc, x) then acc else acc @ [x])
    in
      helper (set2, rm_duplicates_in_list set1)
    end


fun intersect_to_list (set1, set2) =
    let fun helper (set, acc) =
            case set of
                 [] => acc
               | x::x' => helper (x', 
                            if list_contains (set2, x) andalso not
                            (list_contains (acc, x) )
                            then acc @ [x] else acc)
    in
      helper (set1, [])
    end

fun toList intSet =
    case intSet of
         Elems lst => rm_duplicates_in_list lst
       | Range _ => range_to_list intSet
       | Union (set1, set2) => union_to_list (toList set1, toList set2)
       | Intersection (set1, set2) => intersect_to_list (toList set1, toList set2)

fun isEmpty intSet = not (length (toList intSet) > 0)

fun contains (intSet, num) = list_contains (toList intSet, num)
