(* All these practices comes from OCaml 99 Questions *)


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
let slice list b e =
	let rec aux b e acc = function
		| [] -> List.rev acc
		| h::t ->
			match b with
				| n when b > 0 ->
					aux (b-1) (e-1) acc t
				| _ -> 
					match e with
						| n when e >= 0 ->
							aux 0 (n-1) (h::acc) t
						| _  -> List.rev acc in
	if b > (List.length list) then []
	else
		aux b e [] list;;

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






