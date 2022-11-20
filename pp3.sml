fun compose_opt (f, g) = 
    fn x => case g x of
                 NONE => NONE
              | SOME y => f y

fun do_until f p =
    fn x => case p x of
                 true => do_until f p (f x)
              | _ => x

fun factorial n = #2 (do_until (fn (x, acc) => (x - 1, acc * x)) (fn (x, _) => x > 0) (n, 1))

fun fixed_point f x = (do_until f (fn x => f x = x) x)

fun map2 (f, xs) =
    case xs of
         [] => [] 
      | (x, y)::x' => (f x, f y)::(map2 (f, x')) 

fun app_all f g x =
    let val init_ls = g x
        fun helper (xs, acc) =
            case xs of
                 [] => acc
              | x::x' => helper (x', acc @ f x)
    in
      helper (init_ls, [])
    end


fun foldr f acc xs =
    case xs of
         [] => acc
      | x::[] => f (acc, x)
      | x::x' => f (foldr f acc x', x)


fun partition f xs =
    let fun helper (acc1, acc2, ps) =
            case ps of
                 [] => (acc1, acc2)
              | x::x' => if f x 
                         then helper (acc1 @ [x], acc2, x') 
                         else helper (acc1, acc2 @ [x], x')
    in
      helper ([], [], xs)
    end

fun map (f, xs) =
    List.foldr (fn (x, acc) => (f x)::acc) [] xs

fun filter f xs =
    List.foldr (fn (x, acc) => if f x then x::acc else acc) [] xs

fun map f =
    fn xs =>
      case xs of
           [] => []
        | x::x' => (f x)::(map f x')

fun metamap f = map (map f)

datatype 'a tree = leaf | node of { value : 'a, left : 'a tree, right : 'a tree }


