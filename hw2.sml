(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(name : string, names : string list) =
    case names of
         [] => NONE
       | x::x' => case same_string (x, name) of
                       true => SOME (x')
                     | _ => case all_except_option (name, x') of
                                 NONE => NONE
                               | SOME xs => SOME (x::xs)

fun get_substitutions1 (names : string list list, name : string) =
    case names of
         [] => []
       | x::x' => case all_except_option (name, x) of
                       NONE => get_substitutions1 (x', name)
                     | SOME xs => xs @ get_substitutions1 (x', name)

fun get_substitutions2 (names : string list list, name : string) =
    let fun helper (sublist : string list list, acc : string list) =
            case sublist of
                 [] => acc
               | x::x' => case all_except_option (name, x) of
                               NONE => helper (x', acc)
                             | SOME xs => helper (x', acc @ xs)
    in
      helper (names, [])
    end

fun similar_names (names : string list list, name : {first : string, middle : string, last : string}) =
    let val {first = u, middle = v, last = w} = name
        fun helper (filtered_names, acc) =
            case filtered_names of
                 [] => acc
               | x::x' => helper (x', acc @ [{first = x, last = w, middle = v}])
    in
      helper (get_substitutions2 (names, #first name), [name])
    end
      
            
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color card =
    case card of
         (Clubs, _) => Black
       | (Spades, _) => Black
       | _ => Red

fun card_value card =
    case card of
         (_, Ace) => 11
       | (_, Num int) => int
       | _ => 10

fun remove_card (cards, c, e) =
    case cards of
         [] => raise e
       | x::x' => case x = c of
                       true => x'
                     | _ => case remove_card (x', c, e) of
                                 [] => [x]
                               | xs => x::xs

fun all_same_color cards =
    case cards of
         [] => true
       | _::[] => true
       | x::(y::z) => card_color x = card_color y andalso all_same_color (y::z)

fun sum_cards cards =
    let fun helper (cards, acc) =
            case cards of
                 [] => acc
               | x::x' => helper (x', acc + (card_value x))
    in
      helper (cards, 0)
    end

fun score (cards, goal) =
    let fun prelim_score (cards) =
            let val diff = sum_cards cards - goal
            in
              case diff > 0 of
                   true => 3 * diff
                | _ => ~diff
            end
    in
      case (all_same_color cards) of
           true => (prelim_score cards) div 2
         | _ => (prelim_score cards)
    end

fun officiate (cards, moves, goal) =
    let fun helper e =
            case e of
                 ([], _, held) => score (held, goal)
               | (_, [], held) => score (held, goal)
               | (cards, (Discard card)::moves, held) => helper (cards, moves, remove_card (cards, card, IllegalMove))
               | (card::cards, Draw::moves, held) =>
                   let val curr_held = card::held
                       val sum = sum_cards (curr_held)
                   in
                     case sum > goal of
                          true => score (curr_held, goal)
                        | _ => helper (cards, moves, curr_held)
                    end
    in
      helper (cards, moves, [])
    end

fun count_aces (cards, acc) =
    case cards of
         [] => acc
       | (_, Ace)::x' => count_aces (x', 1 + acc)
       | _::x' => count_aces (x', acc)

fun score_challenge (cards, goal) =
    let fun prelim_score (diff) =
            case diff > 0 of
                 true => 3 * diff
              | _ => ~diff
    in
      let val fdiff = sum_cards cards - goal
          val sdiff = (sum_cards cards - goal - (10 * count_aces (cards, 0)))
      in
        case (all_same_color cards) of
             true => (Int.min (prelim_score fdiff, prelim_score sdiff)) div 2
          | _ => Int.min (prelim_score fdiff, prelim_score sdiff)
      end
    end

fun officiate_challenge (cards, moves, goal) =
    let fun helper e =
            case e of
                 ([], _, held) => score_challenge (held, goal)
               | (_, [], held) => score_challenge (held, goal)
               | (cards, (Discard card)::moves, held) => helper (cards, moves, remove_card (cards, card, IllegalMove))
               | (card::cards, Draw::moves, held) =>
                   let val curr_held = card::held
                       val sum = sum_cards (curr_held)
                       val final_sum = Int.min (sum, sum - (10 * count_aces (curr_held, 0)))
                   in
                     case final_sum > goal of
                          true => score_challenge (curr_held, goal)
                        | _ => helper (cards, moves, curr_held)
                    end
    in
      helper (cards, moves, [])
    end

(*fun careful_player (cards, goal) =
    let fun helper (cards, score, acc) =
            case cards of
                 [] => acc
               | x::x' => 
    in
      helper (cards, goal, [])
    end*)
