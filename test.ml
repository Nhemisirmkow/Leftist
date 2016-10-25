open Leftist;;

let iterate w v =
	let rec check command w v =
		match command with
		| "empty" ->
			let w = empty and command = input_line stdin in
					check command w v
		| "add" ->
			let element = float_of_string (input_line stdin) in
				let command = input_line stdin in
					check command (add element w) v
		| "delete_min" ->
			let (min, w) = (delete_min w) in
				let _ = (print_float min; print_newline ()) in
					let command = input_line stdin in
						check command w v
		| "join" ->
			let w = (join w v) and v = empty in
				let command = input_line stdin in
					check command w v
		| "is_empty" ->
			let answer = if is_empty w = true then "TAK" else "NIE" in
				let _ = print_string answer and _ = print_newline () in
					let command = input_line stdin in
						check command w v
		| "swap" ->
			let command = input_line stdin in
				check command v w
		| "exit" ->
			let _ = print_string "Correct exit." in
				false
		| _ ->
			let _ = print_string "Incorrect input." in
				false
			
	in
	let command = input_line stdin in
	let _ = check command w v in
	false
;;

let w = empty and v = empty in
	iterate w v;;
print_newline ();;
print_newline ();;

