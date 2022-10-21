use "pp2.sml";

val pass_student = {id=1, grade=SOME 75};
val fail_student = {id=2, grade=SOME 74};
val absent_student = {id=3, grade=NONE};

val test_pass_or_fail_1 = pass_or_fail pass_student = pass;
val test_pass_or_fail_2 = pass_or_fail fail_student = fail;
val test_pass_or_fail_3 = pass_or_fail absent_student = fail;

val test_has_passed_1 = has_passed pass_student  = true;
val test_has_passed_2 = has_passed fail_student  = false;
val test_has_passed_3 = has_passed absent_student = false;

val test_number_misgraded_1 = number_misgraded [] = 0;
val test_number_misgraded_2 = number_misgraded [(pass, pass_student)] = 0;
val test_number_misgraded_3 = number_misgraded [(fail, pass_student)] = 1;
val test_number_misgraded_4 = number_misgraded [(fail, fail_student)] = 0;
val test_number_misgraded_5 = number_misgraded [(pass, fail_student)] = 1;
val test_number_misgraded_6 = number_misgraded [(fail, fail_student), (pass, pass_student)] = 0;
val test_number_misgraded_7 = number_misgraded [(pass, fail_student), (fail,
pass_student), (pass, absent_student)] = 3;

val test_number_passed_1 = number_passed [] = 0;
val test_number_passed_2 = number_passed [pass_student] = 1;
val test_number_passed_3 = number_passed [pass_student, fail_student, absent_student] = 1;
val test_number_passed_4 = number_passed [pass_student, pass_student] = 2;

val i_tree = leaf;
val two_levels_tree = node {value=1, left=leaf, right=leaf};
val five_levels_tree = node {value=1,
			     left=leaf,
			     right=node {value=2,
					 left=leaf,
					 right=node {value=3,
						     left=leaf,
						     right=node {value=4,
								 left=leaf,
								 right=leaf
								}
						    }
					}
			    };			    

val test_tree_height_1 = tree_height i_tree = 0;
val test_tree_height_2 = tree_height two_levels_tree = 1;
val test_tree_height_3 = tree_height five_levels_tree = 4;

val test_tree_sum_1 = sum_tree i_tree = 0;
val test_tree_sum_2 = sum_tree two_levels_tree = 1;
val test_tree_sum_3 = sum_tree five_levels_tree = 10;

val test_less_than_1 = less_than (int_to_nat 0, int_to_nat 10) = true;
val test_less_than_2 = less_than (int_to_nat 0, int_to_nat 0) = false;
val test_less_than_3 = less_than (int_to_nat 2, int_to_nat 3) = true;
val test_less_than_4 = less_than (int_to_nat 3, int_to_nat 1) = false;

val test_mult_1 = mult (int_to_nat 0, int_to_nat 0) = int_to_nat 0;
val test_mult_2 = mult (int_to_nat 0, int_to_nat 1) = int_to_nat 0;
val test_mult_3 = mult (int_to_nat 1, int_to_nat 0) = int_to_nat 0;
val test_mult_4 = mult (int_to_nat 3, int_to_nat 2) = int_to_nat 6;

val test_sub_1 = sub (int_to_nat 0, int_to_nat 0) = int_to_nat 0;
val test_sub_2 = sub (int_to_nat 1, int_to_nat 0) = int_to_nat 1;
val test_sub_3 = sub (int_to_nat 3, int_to_nat 2) = int_to_nat 1;

val test_add_1 = add(int_to_nat 0, int_to_nat 0) = int_to_nat 0;
val test_add_2 = add(int_to_nat 0, int_to_nat 2) = int_to_nat 2;
val test_add_3 = add(int_to_nat 2, int_to_nat 5) = int_to_nat 7;

val test_int_to_nat_1 = int_to_nat 0 = ZERO;
val test_int_to_nat_2 = int_to_nat 1 = SUCC ZERO;
val test_int_to_nat_3 = int_to_nat 3 = SUCC (SUCC (SUCC ZERO));

val test_pred_1 = pred (SUCC (SUCC ZERO)) = SUCC ZERO;
val test_pred_2 = pred (SUCC ZERO) = ZERO;

val test_is_positive_1 = is_positive ZERO = false;
val test_is_positive_2 = is_positive (SUCC ZERO) = true;

val test_gardener_1 = gardener (node { value = leave_me_alone, left = node { value = prune_me, left = node { value = leave_me_alone, left = leaf, right = leaf }, right = leaf }, right = node { value = leave_me_alone, left = leaf, right = leaf } }) = node { value = leave_me_alone, left = leaf, right = node { value = leave_me_alone, left = leaf, right = leaf } }

val lst_contains_test_1 = list_contains ([1,2,3,4], 4) = true;
val lst_contains_test_2 = list_contains ([], 4) = false;
val lst_contains_test_3 = list_contains ([1,2,3], 4) = false;


val intersection_test_1 = intersect_to_list ([1,2,3], [4,5]) = [];
val intersection_test_2 = intersect_to_list ([1,2,3], [3,4,5]) = [3];
val intersection_test_3 = intersect_to_list ([], []) = [];
val intersection_test_4 = intersect_to_list ([1,2,3], []) = [];
val intersection_test_5 = intersect_to_list ([1,2,3], [0,1,3,5]) = [1,3]

val lst_rm_duplicates_test_1 = rm_duplicates_in_list [1,1,2,3,2] = [1,2,3];
val lst_rm_duplicates_test_2 = rm_duplicates_in_list [1,2,3] = [1,2,3];

val union_test_1 = union_to_list ([1,2,3], [0,1,2]) = [1,2,3,0];
val union_test_2 = union_to_list ([1,2,3], []) = [1,2,3];
val union_test_3 = union_to_list ([], []) = [];
val union_test_4 = union_to_list ([1,1,1,1], []) = [1];

val range_to_lst_test_1 = range_to_list (Range {from = 1, to = 2}) = [1,2];
val range_to_lst_test_2 = range_to_list (Range {from = 2, to = 1}) = [];
val range_to_lst_test_3 = range_to_list (Range {from = 0, to = 0}) = [0];

val toList_test_1 = toList (Elems [1,2,3]) = [1,2,3];
val toList_test_2 = toList (Range {from=1, to=3}) = [1,2,3];
val toList_test_3 = toList (Union (Elems[1], Elems[2,3])) = [1,2,3];
val toList_test_4 = toList (Intersection (Range {from=1, to=10}, Elems [0,1,2,3])) = [1,2,3];
val toList_test_5 = toList (Union (Elems [1,2,3], Elems[0,1,2,3])) = [1,2,3,0];

val test_is_empty_1 = isEmpty (Elems []) = true;
val test_is_empty_2 = isEmpty (Union (Elems [1,2,3], Range {from=1, to=4})) = false;
val test_is_empty_3 = isEmpty (Intersection (Range {from=1, to=10}, Range {from=11, to=20})) = true;
val test_is_empty_4 = isEmpty(Union (Intersection (Range {from=1, to=10}, Elems [5]), Elems [23, 33])) = false;

val test_contains_1 = contains ((Range {from=0, to=10}), 10) = true;
val test_contains_2 = contains ((Elems []), 1) = false;
val test_contains_3 = contains ((Union (Range {from=1, to=10}, Elems[11])), 11) = true;
