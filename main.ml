
class ['a] state init = object(self)
	val mutable state : 'a list = init

	method is_valid = 
		match state with
		| miss :: cann :: boat :: tail -> 
			if miss >= 0 && miss <= 3 && cann >= 0 && cann <= 3 && (boat == 0 || boat == 1) then
				"true"
			else
				"false"
			
		| _ -> "false"

	method is_safe =
		match state with 
		| a :: b :: _ when a = b -> "true"
		| 3 :: _ :: _ -> "true"
		| 0 :: _ :: _ -> "true"
		| _ -> "false"

	method get_state = state

	(* method move move_in = 
		if move_in = "twomissionaries" then
			
		else
			[] *)

end;;



let make_move current how_many boat_loc =
	if boat_loc == 0 then
		current - how_many
	else
		current + how_many

let move move_in state_in = 
	match state_in#get_state with
	| miss :: cann :: boat :: tail ->
 		if move_in == "twomissionaries" then
			new state([make_move miss 2 boat; cann; boat]) 
		else if move_in == "onemissionary" then
			new state([make_move miss 1 boat; cann; boat]) 
		else if move_in == "oneofeach" then
			new state([make_move miss 1 boat; make_move cann 1 boat; boat]) 
		else if move_in == "onecannibal" then
			new state([miss; make_move cann 1 boat; boat]) 
		else if move_in == "twocannibals" then
			new state([miss; make_move cann 2 boat; boat]) 
		else
			state_in
	| _ -> state_in




let something = new state([3;3;0]) ;;
let new_state = move "onemissionary" something
let state_list = new_state#get_state
for num in state_list do
	print_int num
done

(* print_string stuff ;;
print_string "\n" ;;  *)
 


(* let generate_states state_in = 
	let valid_states = [];
	match state_in with
	| miss :: cann :: boat :: tail ->
		if boat = 0 then *)




