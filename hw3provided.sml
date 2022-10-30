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

fun rev_string (str : string) =
    (String.implode o List.rev o String.explode) str
