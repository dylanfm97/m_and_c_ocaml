
class states = object(self)
	val state = [2; 2; 0]


	method is_valid = 
		match state with
		| miss :: cann :: boat :: tail -> 
			if miss >= 0 && miss <= 3 && cann >= 0 && cann <= 3 && (boat == 0 || boat == 1) then
				true
			else
				false
			
		| _ -> false

	method is_safe =
		match state with 
		| a :: b :: _ when a = b -> "true"
		| 3 :: _ :: _ -> "true"
		| 0 :: _ :: _ -> "true"
		| _ -> "false"

end;;

let something = new states ;;
let stuff = something#is_safe ;;
print_string stuff ;;
print_string "\n" ;; 
