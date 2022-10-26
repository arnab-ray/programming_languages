use "hw2.sml";

val all_except_option_test_1 = all_except_option ("string", ["string"]) = SOME []
val all_except_option_test_2 = all_except_option ("string", ["foo"]) = NONE
val all_except_option_test_3 = all_except_option ("foo", ["foo", "bar", "baz"]) = SOME ["bar", "baz"]
val all_except_option_test_4 = all_except_option ("bar", ["foo", "bar", "baz"]) = SOME ["foo", "baz"]
val all_except_option_test_5 = all_except_option ("bar", ["foo", "baz", "bar"]) = SOME ["foo", "baz"]

val get_substitutions1_test_1 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val get_substitutions1_test_2 = get_substitutions1 ([["Fred","Fredrick"],["Freddie","Fred","F"]], "Fred") = ["Fredrick", "Freddie", "F"]
val get_substitutions1_test_3 = get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val get_substitutions1_test_4 = get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty","Fred"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Elizabeth","Betty","Freddie","F"]


val get_substitutions2_test_1 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val get_substitutions2_test_2 = get_substitutions2 ([["Fred","Fredrick"],["Freddie","Fred","F"]], "Fred") = ["Fredrick", "Freddie", "F"]
val get_substitutions2_test_3 = get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val get_substitutions2_test_4 = get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty","Fred"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Elizabeth","Betty","Freddie","F"]

val similar_names_test_1 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"},
         {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"},
         {first="F", last="Smith", middle="W"}]

val card_color_test_1 = card_color (Clubs, Num 2) = Black
val card_color_test_2 = card_color (Spades, Num 2) = Black
val card_color_test_3 = card_color (Diamonds, Num 2) = Red
val card_color_test_4 = card_color (Hearts, Num 2) = Red

val card_value_test_1 = card_value (Clubs, Num 2) = 2
val card_value_test_2 = card_value (Clubs, Ace) = 11
val card_value_test_3 = card_value (Clubs, King) = 10

val remove_card_test_1 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val remove_card_test_2 = remove_card ([(Hearts, Ace), (Diamonds, Num 3)], (Hearts, Ace), IllegalMove) = [(Diamonds, Num 3)]
val remove_card_test_3 = remove_card ([(Diamonds, Num 3), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Diamonds, Num 3)]
val remove_card_test_4 = ((remove_card ([(Hearts, Num 2)], (Hearts, Ace), IllegalMove); false) handle IllegalMove => true)

val all_same_color_test_1 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val all_same_color_test_2 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Clubs, Ace)] = false
val all_same_color_test_3 = all_same_color [] = true

val sum_cards_test_1 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val sum_cards_test_2 = sum_cards [(Clubs, Ace),(Clubs, Num 2)] = 13
val sum_cards_test_3 = sum_cards [] = 0
val sum_cards_test_4 = sum_cards [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)] = 44

val score_test_1 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val score_test_2 = score ([(Hearts, Num 2),(Clubs, Num 4)],2) = 12
val score_test_3 = score ([(Hearts, Num 2),(Clubs, Num 4)],6) = 0
val score_test_4 = score ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],42) = 3
val score_test_5 = score ([],6) = 3

val officiate_test_1 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val officiate_test_2 = officiate ([(Hearts, Num 2),(Clubs, Num 4),(Clubs, Ace)], [Draw, Draw, Draw], 15)
val officiate_test_3 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw, Draw, Draw, Draw, Draw], 42) = 3
val officiate_test_4 = ((officiate([(Clubs,Jack),(Spades,Num(8))], [Draw,Discard(Hearts,Jack)], 42); false) handle IllegalMove => true)
