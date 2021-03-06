(* Eliminate consecutive duplicates of list elements. (medium) *)
(* # compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string list = ["a"; "b"; "c"; "a"; "d"; "e"] *)
let rec compress l =
	let rec comp e = function
		| [] -> [e]
		| h::t ->
			if e = h then
				comp e t
			else
				e::(comp h t)
	in
	match l with
		| [] -> []
		| h::t ->
			comp h t
;;


(* Pack consecutive duplicates of list elements into sublists. (medium) *)
(* # pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
- : string list list =
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]] *)
let rec pack l =
	let rec func e coll l1 l2 =
		match l2 with
			| [] -> coll@[l1]
			| h::t ->
				if e = h then
					func e coll (l1@[h]) t
				else
					if l1 = [] then
						func h coll [] t
					else 
						func h (coll@[l1]) [h] t
	in
	match l with
		| [] -> []
		| h::t ->
			func h [] [h] t
;;


(* Run-length encoding of a list. (easy) *)
(* # encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : (int * string) list =
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] *)
let encode a =
	let rec aux c len coll = function
		| [] -> (coll@[len, c])
		| x :: l ->
			if x = c then
				aux c (len+1) coll l
			else
				aux x 1 (coll@[len, c]) l
	in
	match a with
		| [] -> []
		| x :: l -> 
			aux x 1 [] l
;;

(* Modified run-length encoding. (easy) *)
(* # encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")] *)
type 'a rle =
	| One of 'a
    | Many of int * 'a;;

let encode a =
	let rec aux c len coll = function
		| [] -> 
			if len = 1 then
				(coll@[One c])
			else
				(coll@[Many (len, c)])
			
		| x :: l ->
			if x = c then
				aux c (len+1) coll l
			else
				if len = 1 then
					aux x 1 (coll@[One c]) l
				else
					aux x 1 (coll@[Many (len, c)]) l
	in
	match a with
		| [] -> []
		| x :: l -> 
			aux x 1 [] l
;;

(* Decode a run-length encoded list. (medium) *)
(* # decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;
- : string list =
["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] *)
let decode lst = 
	let rec releaser = function
		| One a ->
			[a]
		| Many (a, b) ->
			if a = 0 then 
				[]
			else
				b :: (releaser (Many (a-1, b)))
	in let rec aux = function
		| [] -> []
		| x :: l ->
			((releaser x)@(aux l))
	in
	aux lst
;;
(* Better *)
let decode list = 
	let rec many acc n x =
		if n = 0 then acc else many (x :: acc) (n-1) x in
	let rec aux acc = function
		| [] -> acc
		| One x :: t -> aux (x :: acc) t
		| Many (n, x) :: t -> aux (many acc n x) t in
	aux [] (List.rev list);;


(* Duplicate the elements of a list. (easy) *)
(* # duplicate ["a";"b";"c";"c";"d"];;
- : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"] *)
let duplicate list = 
	let rec aux repeat output = function
		| [] -> output
		| x :: l ->
			if repeat = true then
				aux false (x::output) (x::l)
			else
				aux true (x::output) l
	in (List.rev (aux true [] list))
;;
(* Better *)
let duplicate = function
	| [] -> []
	| h :: t -> 
		h :: h :: (duplicate t)
;;

(* Replicate the elements of a list a given number of times. (medium) *)
(* # replicate ["a";"b";"c"] 3;;
- : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"] *)
let i = 1;; (* highlight bug *)
let replicate list times =
	let rec aux c = function
			| [] -> []
			| h::t ->
				match c with
					| 0 -> aux times t
					| x -> h::(aux (x-1) (h::t))
	in aux times list
;;

(* Drop every N'th element from a list. (medium) *)
(* # drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
- : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"] *)
let drop list nth =
	let rec aux lst n =
		match lst with
			| [] -> []
			| h::t -> 
				if n = nth then
					(aux t 1)
				else
					h::(aux t (n+1)) in
	aux list 1;;
(* Better *)
let drop list n =
	let rec aux i = function
		| [] -> []
		| h::t -> if i = n then aux 1 t else h :: aux (i+1) t in
	aux 1 list;;


(* Split a list into two parts; the length of the first part is given. (easy) *)
(* # split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
- : string list * string list =
(["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
# split ["a";"b";"c";"d"] 5;;
- : string list * string list = (["a"; "b"; "c"; "d"], []) *)
let split list n =
	let rec aux i head = function
		| [] -> (List.rev head, [])
		| h::t ->
			if i = n then
				((List.rev head), h::t)
			else
				aux (i+1) (h::head) t in
	match list with
		| [] -> ([], [])
		| h::t ->
			aux 1 [h] t;;

(* Better *)
let split list n =
	let rec aux i acc = function
		| [] -> List.rev acc, []
		| h :: t as l ->
			if i = 0 then List.rev acc, l
			else aux (i-1) (h::acc) t in
	aux n [] list


(* Extract a slice from a list. (medium) *)
(* # slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
- : string list = ["c"; "d"; "e"; "f"; "g"] *)
let rec slice list s e = 
	match s with
		| 0 -> 
			if e < 0 then []
			else (List.hd list) :: (slice (List.tl list) s (e-1))
		| _ -> slice (List.tl list) (s-1) (e-1)
;;

(* Better *)
let slice list i k =
	let rec take n = function
		| [] -> []
		| h :: t ->  if n = 0 then [] else h :: take (n-1) t
	in
	let rec drop n = function
		| [] -> []
		| h::t -> as l -> if n = 0 then l else drop (n-1) t
	in
	take (k - i + 1) (drop i list);;


(* Rotate a list N places to the left. (medium) *)
(* # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
- : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
# rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
- : string list = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"] *)
let rotate list ind = 
	let rec aux lst acc i =
		match i with
			| 0 ->
				lst @ (List.rev acc)
			| x when i > 0 ->
				begin
				match lst with
					| [] -> []
					| h::t -> aux t (h::acc) (x-1)
				end
			| x  ->
				match lst with
					| [] -> [] (* Avoid infinite self loop *)
					| h::t -> aux lst acc (x+(List.length lst))
	in
	aux list [] ind
;;


(* SOLVED BY HAND *)
(* Remove the K'th element from a list. (easy) *)
(* # remove_at 1 ["a";"b";"c";"d"];;
- : string list = ["a"; "c"; "d"] *)
let remove_at i list =
	let rec aux i l_list r_list = 
		match r_list with
			| [] ->
				(List.rev l_list)
			| h::t -> 
				if i = 1 then
					(List.rev l_list) @ t
				else if i > 0 then
					aux (i-1) (h::l_list) t
				else
					r_list
	in
	aux i [] list;;


(* SOLVED BY HAND *)
(* Insert an element at a given position into a list. (easy) *)
(* # insert_at "alfa" 1 ["a";"b";"c";"d"];;
- : string list = ["a"; "alfa"; "b"; "c"; "d"]
# insert_at "alfa" 3 ["a";"b";"c";"d"];;
- : string list = ["a"; "b"; "c"; "alfa"; "d"]
# insert_at "alfa" 4 ["a";"b";"c";"d"];;
- : string list = ["a"; "b"; "c"; "d"; "alfa"] *)
let insert_at s i l =
	let rec aux s i ll rl =
		match rl with
			| [] ->
				List.rev (s::ll)
			| h::t ->
				if i = 0 then
					(List.rev (s::ll))@rl
				else
					aux s (i-1) (h::ll) t
	in
	aux s i [] l;;


(* SOLVED BY HAND *)
(* Create a list containing all integers within a given range. (easy) *)
(* # range 4 9;;
- : int list = [4; 5; 6; 7; 8; 9]
# range 9 4;;
- : int list = [9; 8; 7; 6; 5; 4] *)
let range a b =
	let rec aux a b l =
		if a = b then
			List.rev l
		else if a > b then
			aux (a-1) b (a::l)
		else
			aux (a+1) b (a::l)
	in
	aux a b [];;


(* Extract a given number of randomly selected elements from a list. (medium) *)
(* # rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3;;
- : string list = ["g"; "d"; "a"] *)
let rec rand_select list n =
    let rec extract acc n = function
      | [] -> raise Not_found
      | h :: t -> if n = 0 then (h, acc @ t) else extract (h::acc) (n-1) t
    in
    let extract_rand list len =
      extract [] (Random.int len) list
    in
    let rec aux n acc list len =
      if n = 0 then acc else
        let picked, rest = extract_rand list len in
        aux (n-1) (picked :: acc) rest (len-1)
    in
    let len = List.length list in
    aux (min n len) [] list len;;


