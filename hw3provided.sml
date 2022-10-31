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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals (strs : string list) =
    List.filter (fn str => Char.isUpper (String.sub (str, 0))) strs

fun longest_string1 (strs : string list) =
    List.foldl (fn (x, y) => if (String.size y) >= (String.size x) then y else x) "" strs

fun longest_string2 (strs : string list) =
    List.foldl (fn (x, y) => if (String.size y) > (String.size x) then y else x) "" strs

fun longest_capitalized (strs : string list) =
    (longest_string1 o only_capitals) strs

fun longest_string_helper predicate xs = 
  case xs of
       [] => ""
    | x::[] => x
    | x::x' => 
        let val highest_tail = longest_string_helper predicate x'
        in
          if predicate (String.size x, String.size highest_tail) then x else highest_tail
        end

fun longest_string3 (strs : string list) =
    longest_string_helper (fn (x,y) => x >= y) strs

fun longest_string4 (strs : string list) =
    longest_string_helper (fn (x,y) => x > y) strs

fun rev_string (str : string) =
    (String.implode o List.rev o String.explode) str

fun first_answer f xs =
    case xs of
         [] => raise NoAnswer
      | x::x' => case f x of
                      SOME v => v
                    | _ => first_answer f x'

fun all_answers f xs =
    let fun helper (acc, ys) =
            case ys of
                 [] => SOME acc
               | y::y' => case f y of
                               NONE => NONE
                             | SOME v => helper (v @ acc, y')
    in
      helper ([], xs)
    end

(*fun count_wildcards p =*)
    
