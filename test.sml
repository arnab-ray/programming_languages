fun fold (f,acc,xs) =
	case xs of
		[] => acc
	 | x::xs' => fold (f, f(acc,x), xs')

fun fold2 f = fn acc => fn xs =>
  case xs of
       [] => acc
    | x::xs' => fold2 f (f(acc,x)) xs'

fun has_repeat xs =
    case xs of
         [] => false
      | x::x' => (List.exists (fn y => x = y) x') orelse (has_repeat x') 
