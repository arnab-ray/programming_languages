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

fun longest_string_helper predicate xs =
    List.foldl (fn (x, y) => if predicate (x, y) then x else y) "" xs

fun longest_string3 (strs : string list) =
    longest_string_helper (fn (x,y) => x >= y) strs

fun longest_string4 (strs : string list) =
    longest_string_helper (fn (x,y) => x > y) strs

fun longest_capitalized (strs : string list) =
    (longest_string1 o only_capitals) strs

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

fun count_wildcards p = g (fn () => 1) (fn x => 0) p 

fun count_wild_and_variable_lengths p = g (fn () => 1) (fn x => String.size x) p

fun count_some_var (s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
    let fun get_strings (x) =
            case x of
                 Variable b => [b]
              | TupleP ps => List.foldl (fn (x, y) => (get_strings x) @ y) [] ps
              | ConstructorP(_, x) => get_strings x
              | _ => []
        fun has_repeat xs =
            case xs of
                 [] => false
              | x::x' => (List.exists (fn y => x = y) x') orelse (has_repeat x')
    in
      not (has_repeat (get_strings p))
    end

fun match (v : valu, p : pattern) =
    case p of
         Wildcard => SOME []
       | Variable s => SOME [(s, v)]
       | UnitP => 
           (case v of
                 Unit => SOME []
              | _ => NONE
           )
       | ConstP i => 
           (case v of
                 Const u => if i = u then SOME [] else NONE
              | _ => NONE
           )
       | TupleP ps => 
           (case v of
                 Tuple vs => if List.length ps = List.length vs 
                             then all_answers match (ListPair.zip (vs, ps)) 
                             else NONE
              | _ => NONE
           )
       | ConstructorP (s1, ps) => 
           (case v of
                 Constructor(s2, vs) => if s1 = s2 then match (vs, ps) else NONE
              | _ => NONE
           )

fun first_match v ps =
    SOME (first_answer (fn p => match (v, p)) ps) handle NoAnswer => NONE
