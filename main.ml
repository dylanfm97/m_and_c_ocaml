
let print_list state_in = 
	let print_number num = Printf.printf "%d " num in 
	List.iter print_number state_in;
	Printf.printf "\n"

class ['a] state init = object(self)
	val mutable state : 'a list = init

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
		| a :: b :: _ when a = b -> true
		| 3 :: _ :: _ -> true
		| 0 :: _ :: _ -> true
		| _ -> false

	method is_goal = 
		match state with
		| 0 :: 0 :: _ -> true
		| _ -> false

	method get_state = state

	method print_state =
		print_list state
end;;

let move_boat boat =
	if boat = 0 then
		1
	else
		0

let make_move current how_many boat_loc =
	if boat_loc == 0 then
		current - how_many
	else
		current + how_many

let move move_in state_in = 
	match state_in#get_state with
	| miss :: cann :: boat :: tail ->
 		if move_in = "twomissionaries" then
			new state([make_move miss 2 boat; cann; move_boat boat]) 
		else if move_in = "onemissionary" then
			new state([make_move miss 1 boat; cann; move_boat boat]) 
		else if move_in = "oneofeach" then
			new state([make_move miss 1 boat; make_move cann 1 boat; move_boat boat]) 
		else if move_in = "onecannibal" then
			new state([miss; make_move cann 1 boat; move_boat boat]) 
		else if move_in = "twocannibals" then
			new state([miss; make_move cann 2 boat; move_boat boat]) 
		else
			new state([0;0;0])
	| _ -> new state([0;0;0])




let generate_states state_in = 
	[move "onemissionary" state_in; move "twomissionaries" state_in; move "oneofeach" state_in; move "onecannibal" state_in; move "twocannibals" state_in] 


let print_generated_states states_in = 
	let print_state state_in = state_in#print_state in
	List.iter print_state states_in ;;


let rec _search state_in max_depth current_depth = 
	let new_states = generate_states state_in in
	let verify this_state = 
		if this_state#is_goal then
			Printf.printf "goal found!\n"
(* 			this_state#print_state;
 *)(* 			yay;
 *)		else if this_state#is_valid && this_state#is_safe && current_depth < max_depth then
(* 			this_state#print_state; 
 *)			let new_depth = current_depth + 1 in
			_search this_state max_depth new_depth;
(* 			this_state#print_state 
 *)(*  			this_state#print_state 
 *) in 
	List.iter verify new_states ;;

let search state_in max_depth = 
	_search state_in max_depth 0

(* let rec search =  *)
let something = new state([3;3;0]) ;;
(* let the_state = something#get_state ;;
let new_states = generate_states something ;;
print_generated_states new_states ;; *)

search something 10 ;;


(* let print_it num = print_int num in
List.iter print_it the_state ;; *)

(* List.iter (fun s -> print_int s) the_state ;; *)

(* let a_list = ["hello"; "world"] in
List.iter (fun s -> print_endline s) a_list ;; *)



(* let s elem = elem in
List.iter s new_states ;; *)


(* let something = new state([3;3;0]) ;;
let new_state = move "onemissionary" something ;;
let state_list = new_state#get_state ;;

let other_state = move "twocannibals" new_state ;;
let this_state_list = other_state#get_state ;;
 *) 






