(* Luca Lombardo, Mat. 546688 *)

open List;;

(* Linguaggio *)
type ide = string;;  (* identificatore *)

type exp = 
	| CstInt of int (* costante intera *)
	| CstTrue (* costante booleana true *)
	| CstFalse (* costante booleana false *)
	| Den of ide (* sostituisce a ide il suo valore *)
	| Sum of exp * exp (* somma tra interi *)
	| Sub of exp * exp (* sottrazione tra interi *)
	| Times of exp * exp (* moltiplicazione tra interi *)
	| Ifthenelse of exp * exp * exp (* operatore ternario ifthenelse *)
	| Eq of exp * exp (* equivalenza tra ide *)
	| Let of ide * exp * exp (* assegnamento *)
	| Fun of ide * exp (* funzione unaria non ricorsiva *)
	| Letrec of ide * ide * exp * exp (* funzione unaria ricorsiva *)
	| Apply of exp * exp (* applicazione di funzione *)
	(* Inizio progetto *)
	| EString of string  (* costante stringa *)
	| Empty of setType (* restituisce un insieme vuoto tipato *)
	| Singleton of setType * exp (* restituisce un insieme con un elemento *)
	| Of of setType * exp list (* restituisce un insieme con una lista di elementi *)
	(* Operazioni di base su insiemi *)
	| Union of exp * exp (* operazione di unione tra insiemi *)
	| Intersec of exp * exp (* operazione di intersezione tra insiemi *)
	| Diff of exp * exp (* operazione di differenza tra insiemi *)
	| Add of exp * exp (* aggiunge un elemento ad un insieme *)
	| Remove of exp * exp (* rimuove un elemento da un insieme *)
	| IsEmpty of exp (* restituisce true se l'insieme è vuoto, false altrimenti *)
	| Member of exp * exp (* restituisce true se l'elemento appartiene all'insieme, false altrimenti *)
	| Subset of exp * exp (* restituisce true se il primo insieme è sottoinsieme del secondo, false altrimenti *)
	| Min of exp (* restituisce l'elemento con valore minimo all'interno dell'insieme *)
	| Max of exp (* restituisce l'elemento con valore massimo all'interno dell'insieme *)
	(* Operatori funzionali su insiemi *)
	| For_all of exp * exp (* controlla se tutti gli elementi dell'insieme soddisfano il predicato *)
	| Exists of exp * exp (* controlla se esiste almeno un elemento dell'insieme che soddisfa il predicato *)
	| Filter of exp * exp (* restituisce l'insieme degli elementi appartenenti all'insieme dato che soddisfano il predicato *)
	| Map of exp * exp (* restituisce l'insieme di valori v = f(x) dove f è una funzione data e x appartiene all'insieme dato *)
	(* Possibili tipi degli insiemi *)
and setType =
	| IntSet
	| BoolSet
	| StringSet
;;

(* Ambiente polimorfo *)
type 'v env = (string * 'v) list;; 
type evT = (* tipi esprimibili *)
	| Int of int
	| Bool of bool  
	| Closure of ide * exp * evT env  (* chiusura *)
	| RecClosure of ide * ide * exp * evT env (* chiusura ricorsiva *)
	| Unbound
	(* Inizio progetto *)
	| String of string
	| Set of setType * evSet
and evSet = 
	| EmptySet
	| Elements of evT list
;;

let emptyEnv  = [("", Unbound)];;
let bind (s: evT env) (i: string) (x: evT) = (i, x) :: s;; (* binding *)
let rec lookup (s: evT env) (i: string) = match s with (* ricerca i nell'ambiente s *)
	| [] ->  Unbound
	| (j,v)::sl when j = i -> v
	| _::sl -> lookup sl i
;;

let rec typecheck (s, v) : bool = match s with	(* typechecker *)
	| "int" -> (match v with 
		      | Int(u) -> true
		      | _ -> false)
	| "bool" -> (match v with 
			| Bool(u) -> true
			| _ -> false)
	(* Inizio progetto *)
	| "string" -> (match v with 
			| String(_) -> true
			| _ -> false)
	| "set" -> (match v with
			| Set(_) -> true
			| _ -> false)
	| _ -> failwith ("not a valid type")
;;

(* Operazioni primitive *)
let int_eq(x,y) =
	match (typecheck("int",x), typecheck("int",y), x, y) with
	       | (true, true, Int(v), Int(w)) -> Bool(v = w)
	       | (_,_,_,_) -> failwith("run-time error")
;;
       
 let int_plus(x, y) =
	match(typecheck("int",x), typecheck("int",y), x, y) with
	       | (true, true, Int(v), Int(w)) -> Int(v + w)
	       | (_,_,_,_) -> failwith("run-time error")
;;

let int_sub(x, y) =
	match(typecheck("int",x), typecheck("int",y), x, y) with
	       | (true, true, Int(v), Int(w)) -> Int(v - w)
	       | (_,_,_,_) -> failwith("run-time error")
;;

let int_times(x, y) =
	match(typecheck("int",x), typecheck("int",y), x, y) with
	       | (true, true, Int(v), Int(w)) -> Int(v * w)
	       | (_,_,_,_) -> failwith("run-time error")
;;

(* Funz. ausiliari su liste *)
(* Restituisce true se el occorre in lis, false altrimenti *)
let rec exist (el: 'a) (lis: 'a list) : bool =
	match lis with
	       | [] -> false
	       | h::tl -> (el = h) || (exist el tl)	
;;

(* Restituisce true se lis non contiene elementi duplicati, false altrimenti *)
let rec noDup (lis: 'a list) : bool =
	match lis with
		| [] -> true
		| h::tl -> (not (exist h tl)) && (noDup tl)
;;

(* Rimuove gli elementi duplicati da una lista *)
let rec removeDup (lis: 'a list) : 'a list =
	match lis with
		| [] -> []
		| h::tl -> 
			if (exist h tl) then removeDup tl 
			else h::(removeDup tl)
;;

(* Unione fra due liste *)	
let rec union_list (lis1: 'a list) (lis2: 'a list) : 'a list =
	match (lis1, lis2) with
	       | ([], _) -> lis2
	       | (x::xs, y) -> 
		      if (not (exist x y)) then x::(union_list xs y) 
			else union_list xs y
;;

(* Intersezione fra due liste *)
let rec intersec_list (lis1: 'a list) (lis2: 'a list) : 'a list = 
	match (lis1, lis2) with
	       | ([], _) -> []
	       | (x::xs, y) -> 
			if (exist x y) then x::(intersec_list xs y) 
			else intersec_list xs y
;;

(* Differenza fra due liste *)
let rec diff_list (lis1: 'a list) (lis2: 'a list) : 'a list =
	match (lis1, lis2) with
	       | ([], _) -> []
               | (x::xs, y) -> 
			if (exist x y) then diff_list xs y 
			else x::(diff_list xs y)
;;

(* Rimuove tutte le occorrenze di un elemento in una lista data *)
let rec remove_list (lis: 'a list) (el: 'a) : 'a list =
	match lis with
		| [] -> lis
		| h::tl -> 
			if (h = el) then remove_list tl el 
			else h::(remove_list tl el)
;;

(* Restituisce true se tutti gli elementi di lis1 occorrono in lis2, false altrimenti *)
let rec subset_list (lis1: 'a list) (lis2: 'a list) : bool =
	match (lis1, lis2) with
		| ([], y) -> true
		| (x::xs, y) -> 
			if (exist x y) then subset_list xs y 
			else false
;;

(* Restituisce il minimo valore all'interno di una lista *)
let min_list (lis: 'a list) : 'a =
	match lis with
		| [] -> failwith("empty list")
		| h::tl -> List.fold_left min h tl
;;

(* Restituisce il massimo valore all'interno di una lista *)
let max_list (lis: 'a list) : 'a =
	match lis with
		| [] -> failwith("empty list")
		| h::tl -> List.fold_left max h tl
;;

(* Interprete *)
let rec eval  (e: exp) (s: evT env) =
	match e with
		| CstInt(n) -> Int(n)
		| CstTrue -> Bool(true)
		| CstFalse -> Bool(false)
		| Eq(e1, e2) ->
			int_eq((eval e1 s), (eval e2 s))
		| Times(e1,e2) ->
			int_times((eval e1 s), (eval e2 s))
		| Sum(e1, e2) ->
			int_plus((eval e1 s), (eval e2 s))
		| Sub(e1, e2) ->
			int_sub((eval e1 s), (eval e2 s))
		| Ifthenelse(e1,e2,e3) ->
			let g = eval e1 s in
				let ev2 = eval e2 s in
					let ev3 = eval e3 s in 
						(if (same_type ev2 ev3) then (* estensione: controllo sui tipi del risultato *)
							(match (typecheck("bool", g), g) with
								| (true, Bool(true)) -> ev2
								| (true, Bool(false)) -> ev3
								| (_, _) -> failwith ("nonboolean guard"))
						else failwith("branches must have the same type"))
		| Den(i) -> 
			lookup s i
		| Let(i, e, ebody) ->
			eval ebody (bind s i (eval e s))
		| Fun(arg, ebody) ->
			Closure(arg, ebody, s)
		| Letrec(f, arg, fBody, letBody) ->
			let benv = bind (s) (f) (RecClosure(f, arg, fBody, s)) in eval letBody benv
		| Apply(eF, eArg) ->
			let fclosure = eval eF s in 
				(match fclosure with 
					| Closure(arg, fbody, fDecEnv) ->
						let aVal = eval eArg s in
							let aenv = bind fDecEnv arg aVal in eval fbody aenv
					| RecClosure(f, arg, fbody, fDecEnv) ->
						let aVal = eval eArg s in
							let rEnv = bind fDecEnv f fclosure in
								let aenv = bind rEnv arg aVal in eval fbody aenv
					| _ -> failwith("non functional value"))
		(* Inizio progetto *)
		| EString(st) -> String(st)
		(* Restituisce un insieme vuoto di tipo typ *)
		| Empty(typ) -> Set(typ, EmptySet)
		(* Restituisce un insieme contenente l'elemento e *)
		| Singleton(typ, e) -> 
			if (checkElements typ [e] s) then Set(typ, eval_els [e] s)
			else failwith("set is not correctly typed")
		(* Restituisce un insieme contenenti gli elementi nella lista lis *)
		| Of(typ, lis) -> 
			if (checkElements typ lis s) then 
				(if (noDup lis) then Set(typ, eval_els lis s) 
				else failwith("a set can't contain duplicate elements"))
			else failwith("set is not correctly typed")
		(* Restituisce i1 ∪ i2
		   Fallisce se: i1 o i2 non sono insiemi || i1 e i2 hanno elementi di tipo diverso *)
		| Union(i1, i2) -> 
			let ev1 = eval i1 s in 
				let ev2 = eval i2 s in
					(match (typecheck("set", ev1), typecheck("set", ev2), ev1, ev2) with
						| (true, true, Set(t1, e1), Set(t2, e2)) -> 
							if (t1 = t2) then (match (e1, e2) with
										| (EmptySet, _) -> ev2
										| (_, EmptySet) -> ev1
										| (Elements(lis1), Elements(lis2)) -> Set(t1, Elements(union_list lis1 lis2)))
							else failwith("sets have different types")
						| (_, _, _, _) -> failwith("not a set"))
		(* Restituisce i1 ∩ i2
		   Fallisce se: i1 o i2 non sono insiemi || i1 e i2 hanno elementi di tipo diverso *)
		| Intersec(i1, i2) -> 
			let ev1 = eval i1 s in 
				let ev2 = eval i2 s in
					(match (typecheck("set", ev1), typecheck("set", ev2), ev1, ev2) with
						| (true, true, Set(t1, e1), Set(t2, e2)) -> 
							if (t1 = t2) then (match (e1, e2) with
										| (EmptySet, _) -> ev1
										| (_, EmptySet) -> ev2
										| (Elements(lis1), Elements(lis2)) -> 
											let res = intersec_list lis1 lis2 in
												(match res with
													| [] -> Set(t1, EmptySet)
													| _ -> Set(t1, Elements(res))))
							else failwith("sets have different types")
						| (_, _, _, _) -> failwith("not a set"))
		(*  Restituisce i1 − i2
		    Fallisce se: i1 o i2 non sono insiemi || i1 e i2 hanno elementi di tipo diverso *)
		| Diff(i1, i2) -> 
			let ev1 = eval i1 s in 
				let ev2 = eval i2 s in
					(match (typecheck("set", ev1), typecheck("set", ev2), ev1, ev2) with
						| (true, true, Set(t1, e1), Set(t2, e2)) ->
					        	if (t1 = t2) then (match (e1, e2) with
										| (EmptySet, _) -> ev1
										| (_, EmptySet) -> ev1
										| (Elements(lis1), Elements(lis2)) -> 
											let res = diff_list lis1 lis2 in
												(match res with
													| [] -> Set(t1, EmptySet)
													| _ -> Set(t1, Elements(res))))
							else failwith("sets have different types")
						| (_, _, _, _) -> failwith("not a set"))
		(*  Aggiunge l'elemento e all'insieme i se non è già presente
		    Fallisce se: i non è un insieme || e ha tipo diverso rispetto agli elementi di i *)
		| Add(i, e) -> 
			let iv = eval i s in
				let ev = eval e s in
					(match (typecheck("set", iv), iv) with 
						| (true, Set(t, e)) -> 
							(match t with
								| IntSet -> 
									if (typecheck("int", ev)) then 
										(match e with 
											| EmptySet -> Set(t, Elements([ev]))
											| Elements(lis) -> 
												if (not (exist ev lis)) then Set(t, Elements(ev::lis))
												else iv)
									else failwith("element and set have different types")
								| BoolSet -> 
									if (typecheck("bool", ev)) then 
										(match e with 
											| EmptySet -> Set(t, Elements([ev]))
											| Elements(lis) -> 
												if (not (exist ev lis)) then Set(t, Elements(ev::lis))
												else iv)
									else failwith("element and set have different types")
																																			 
								| StringSet -> 
									if (typecheck("string", ev)) then 
										(match e with 
											| EmptySet -> Set(t, Elements([ev]))
											| Elements(lis) -> 
												if (not (exist ev lis)) then Set(t, Elements(ev::lis))
												else iv)
									else failwith("element and set have different types"))
						| (_, _) -> failwith("not a set"))
		(*  Rimuove l'elemento e dall'insieme i, se presente
		    Fallisce se: i non è un insieme || e ha tipo diverso rispetto agli elementi di i *)
		| Remove(i, e) -> 
			let iv = eval i s in
				let ev = eval e s in
					(match (typecheck("set", iv), iv) with 
						| (true, Set(t, e)) -> 
							(match t with
								| IntSet -> 
									if (typecheck("int", ev)) then 
										(match e with 
											| EmptySet -> iv
											| Elements(lis) -> 
												if (exist ev lis) then 
													let res = remove_list lis ev in
														(match res with
															| [] -> Set(t, EmptySet)
															| _ -> Set(t, Elements(res)))
												else iv)					 
									else failwith("element and set have different types")
								| BoolSet ->
									if (typecheck("bool", ev)) then 
										(match e with 
											| EmptySet -> iv
											| Elements(lis) -> 
												if (exist ev lis) then 
													let res = remove_list lis ev in
														(match res with
															| [] -> Set(t, EmptySet)
															| _ -> Set(t, Elements(res)))
												else iv)	
									else failwith("element and set have different types")										 
								| StringSet -> 
									if (typecheck("string", ev)) then
										(match e with 
											| EmptySet -> iv
											| Elements(lis) -> 
												if (exist ev lis) then 
													let res = remove_list lis ev in
														(match res with
															| [] -> Set(t, EmptySet)
															| _ -> Set(t, Elements(res)))
												else iv)	
									else failwith("element and set have different types"))
						| (_, _) -> failwith("not a set"))
		(*  Restituisce true se l'insieme i è vuoto, false altrimenti
		    Fallisce se: i non è un insieme *)
		| IsEmpty(i) -> 
			let iv = eval i s in
				(match (typecheck("set", iv), iv) with
					| (true, Set(t, e)) -> 
						(match e with
							| EmptySet -> Bool(true)
							| Elements(lis) -> Bool(false))
					| (_, _) -> failwith("not a set"))
		(*  Restituisce true se l'insieme i contiene l'elemento e, false altrimenti
		    Fallisce se: i non è un insieme || e ha tipo diverso rispetto agli elementi di i *)
		| Member(i, e) ->
			let iv = eval i s in
				let ev = eval e s in
					(match (typecheck("set", iv), iv) with 
						| (true, Set(t, e)) -> 
							(match t with
								| IntSet -> 
									if (typecheck("int", ev)) then 
										(match e with 
											| EmptySet -> Bool(false)
											| Elements(lis) -> let res = exist ev lis in Bool(res))
									else failwith("element and set have different types")
								| BoolSet -> 
									if (typecheck("bool", ev)) then 
										(match e with 
											| EmptySet -> Bool(false)
											| Elements(lis) -> let res = exist ev lis in Bool(res))
									else failwith("element and set have different types")			 
								| StringSet ->
									if (typecheck("string", ev)) then 
										(match e with 
											| EmptySet -> Bool(false)
											| Elements(lis) -> let res = exist ev lis in Bool(res))
									else failwith("element and set have different types"))
						| (_, _) -> failwith("not a set"))
		(*  Restituisce true se l'insieme i1 è sottoinsieme dell'insieme i2
		    Fallisce se: i1 o i2 non sono insiemi || i1 e i2 hanno elementi di tipo diverso *)
		| Subset(i1, i2) -> 
			let ev1 = eval i1 s in 
				let ev2 = eval i2 s in
					(match (typecheck("set", ev1), typecheck("set", ev2), ev1, ev2) with
						| (true, true, Set(t1, e1), Set(t2, e2)) -> 
							if (t1 = t2) then (match (e1, e2) with
										| (EmptySet, _) -> Bool(true)
										| (Elements(lis1), EmptySet) -> Bool(false)
										| (Elements(lis1), Elements(lis2)) -> 
											let res = subset_list lis1 lis2 in Bool(res))																			
							else failwith("sets have different types")
						| (_, _, _, _) -> failwith("not a set"))
		(*  Restituisce l'elemento dal valore minimo presente all'interno dell'insieme i
		    Utilizza l'ordinamento naturale di OCaml; quindi per i valori di tipo Bool si ha false < true
		    Fallisce se: i non è un insieme || i è un insieme vuoto *)
		| Min(i) -> 
			let iv = eval i s in
				(match (typecheck("set", iv), iv) with
					| (true, Set(t, e)) -> 
						(match e with
							| EmptySet -> failwith("empty set")
							| Elements(lis) -> min_list lis)
					| (_, _) -> failwith("type error"))
		(*  Restituisce l'elemento dal valore massimo presente all'interno dell'insieme i
		    Utilizza l'ordinamento naturale di OCaml; quindi per i valori di tipo Bool si ha false < true
		    Fallisce se: i non è un insieme || i è un insieme vuoto *)
		| Max(i) -> 
			let iv = eval i s in
				(match (typecheck("set", iv), iv) with
					| (true, Set(t, e)) -> 
						(match e with
							| EmptySet -> failwith("empty set")
							| Elements(lis) -> max_list lis)
					| (_, _) -> failwith("not a set"))
		(*  Restituisce true se per ogni e appartenente ad i, p(e) = true, false altrimenti
		    Fallisce se: p non è un predicato || i non è un set *)
		| For_all(p, i) -> 
			let ep = eval p s in
				let iv = eval i s in
					(match (ep, typecheck("set", iv), iv) with
						| (Closure(arg, fbody, fDecEnv), true, Set(t, e)) -> 
							(match e with 
								| EmptySet -> Bool(true)
								| Elements(lis) -> for_all_aux ep lis)
						| (RecClosure(f, arg, fbody, fDecEnv), true, Set(t, e)) ->
							(match e with 
								| EmptySet -> Bool(true)
								| Elements(lis) -> for_all_aux ep lis)
						| (_, _, _) -> failwith("the two arguments are not a predicate and a set"))
		(*  Restituisce true se esiste e appartenente ad i . p(e) = true, false altrimenti
		    Fallisce se: p non è un predicato || i non è un set *)
		| Exists(p, i) -> 
			let ep = eval p s in
				let iv = eval i s in
					(match (ep, typecheck("set", iv), iv) with
						| (Closure(arg, fbody, fDecEnv), true, Set(t, e)) -> 
							(match e with 
								| EmptySet -> Bool(false)
								| Elements(lis) -> exists_aux ep lis)
						| (RecClosure(f, arg, fbody, fDecEnv), true, Set(t, e)) -> 
							(match e with 
								| EmptySet -> Bool(false)
								| Elements(lis) -> exists_aux ep lis)
						| (_, _, _) -> failwith("the two arguments are not a predicate and a set"))
		(*  Restituisce l'insieme degli elementi appartenenti ad i che soddisfano il predicato p
		    Fallisce se: p non è un predicato || i non è un set *)
		| Filter(p, i) -> 
			let ep = eval p s in
				let iv = eval i s in
					(match (ep, typecheck("set", iv), iv) with
						| (Closure(arg, fbody, fDecEnv), true, Set(t, e)) -> 
							(match e with 
								| EmptySet -> iv
								| Elements(lis) -> 
									let res = filter_aux ep lis in 
										(match res with
											| [] -> Set(t, EmptySet)
											| _ -> Set(t, Elements(res))))
						| (RecClosure(f, arg, fbody, fDecEnv), true, Set(t, e)) -> 
							(match e with 
								| EmptySet -> iv
								| Elements(lis) -> 
									let res = filter_aux ep lis in 
										(match res with
											| [] -> Set(t, EmptySet)
											| _ -> Set(t, Elements(res))))
						| (_, _, _) -> failwith("the two arguments are not a predicate and a set"))
		(*  Restituisce l'insieme dei valori v tali che v = f(x) dove x appartiene ad i
		    Fallisce se: p non è una funzione || i non è un set *)
		| Map(f, i) -> 
			let ef = eval f s in
				let iv = eval i s in
					(match (ef, typecheck("set", iv), iv) with
						| (Closure(arg, fbody, fDecEnv), true, Set(t, e)) -> 
							let typ = find_type ef t in 
								(match typ with
									| Int(_) -> 
										(match e with
											| EmptySet -> Set(IntSet, EmptySet)
											| Elements(lis) -> 
												let res = map_aux ef lis in 
													(match res with
														| [] -> Set(IntSet, EmptySet)
														| x::xs -> Set(IntSet, Elements(removeDup res))))
									| Bool(_) -> 
										(match e with
											| EmptySet -> Set(BoolSet, EmptySet)
											| Elements(lis) -> 
												let res = map_aux ef lis in 
													(match res with
														| [] -> Set(BoolSet, EmptySet)
														| x::xs -> Set(BoolSet, Elements(removeDup res))))
									| String(_) -> 
										(match e with
											| EmptySet -> Set(StringSet, EmptySet)
											| Elements(lis) -> 
												let res = map_aux ef lis in 
													(match res with
														| [] -> Set(StringSet, EmptySet)
														| x::xs -> Set(StringSet, Elements(removeDup res))))
											| _ -> failwith("type error"))
						| (RecClosure(f, arg, fbody, fDecEnv), true, Set(t, e)) -> 
							let typ = find_type ef t in 
								(match typ with
									| Int(_) -> 
										(match e with
											| EmptySet -> Set(IntSet, EmptySet)
											| Elements(lis) -> 
												let res = map_aux ef lis in 
													(match res with
														| [] -> Set(IntSet, EmptySet)
														| x::xs -> Set(IntSet, Elements(removeDup res))))
									| Bool(_) -> 
										(match e with
											| EmptySet -> Set(BoolSet, EmptySet)
											| Elements(lis) -> 
												let res = map_aux ef lis in 
													(match res with
														| [] -> Set(BoolSet, EmptySet)
														| x::xs -> Set(BoolSet, Elements(removeDup res))))
									| String(_) -> 
										(match e with
											| EmptySet -> Set(StringSet, EmptySet)
											| Elements(lis) -> 
												let res = map_aux ef lis in 
													(match res with
														| [] -> Set(StringSet, EmptySet)
														| x::xs -> Set(StringSet, Elements(removeDup res))))
									| _ -> failwith("type error"))
						| (_, _, _) -> failwith("the two arguments are not a function and a set"))
(* Applica la eval ad ogni elemento di una lista di exp *)
and eval_els (els: exp list) (s: evT env) : evSet =
	match els with
		| [] -> EmptySet
		| x::xs -> 
			let eval_el el = eval el s in 
				let res = List.map eval_el els in Elements(res)
(* Controlla che l'insieme abbia tutti gli elementi del tipo corretto *)
and checkElements (t: setType) (lis: exp list) (s: evT env) : bool =
	match t with 
		| IntSet -> (match lis with 
				| [] -> true
				| h::tl -> let ev = eval h s in 
						(typecheck("int", ev) && (checkElements t tl s)))
		| BoolSet -> (match lis with 
				| [] -> true
				| h::tl -> let ev = eval h s in 
						(typecheck("bool", ev) && (checkElements t tl s)))
		| StringSet -> (match lis with 
				| [] -> true
				| h::tl -> let ev = eval h s in 
						(typecheck("string", ev) && (checkElements t tl s)))
(* Funzione ausiliaria della For_all *)
and for_all_aux (fclosure: evT) (lis: evT list) : evT =
	match lis with
		| [] -> Bool(true)
		| h::tl -> let app = apply_evt fclosure h in
				(match app with
					| Bool(true) -> for_all_aux fclosure tl
					| Bool(false) -> Bool(false)
					| _ -> failwith("non boolean function"))
(* Funzione ausiliaria della Exists *)
and exists_aux (fclosure: evT) (lis: evT list) : evT =
	match lis with
		| [] -> Bool(false)
		| h::tl ->  let app = apply_evt fclosure h in
				(match app with
					| Bool(true) -> Bool(true)
					| Bool(false) -> exists_aux fclosure tl
					| _ -> failwith("non boolean function"))
(* Funzione ausiliaria della Filter *)
and filter_aux (fclosure: evT) (lis: evT list) : evT list =
	match lis with
		| [] -> []
		| h::tl -> let app = apply_evt fclosure h in
				(match app with
					| Bool(true) -> h::(filter_aux fclosure tl)
					| Bool(false) -> filter_aux fclosure tl
					| _ -> failwith("non boolean function"))
(* Funzione ausiliaria della Map *)
and map_aux (fclosure: evT) (lis: evT list) : evT list =
	match lis with
		| [] -> []
		| h::tl -> let app = apply_evt fclosure h in app::(map_aux fclosure tl)
(* Funzione ausiliaria che prende la chiusura di una funzione, la applica a un evT e restituisce il risultato *)
and apply_evt (fclosure: evT) (e: evT) : evT =
	match fclosure with
		| Closure(arg, fbody, fDecEnv) -> let aenv = bind fDecEnv arg e in eval fbody aenv
		| RecClosure(f, arg, fbody, fDecEnv) ->	let rEnv = bind fDecEnv f fclosure in
								let aenv = bind rEnv arg e in eval fbody aenv
		| _ -> failwith("non functional value")	
(*  Funzione ausiliaria che controlla che due evT abbiano lo stesso tipo. 
	Usata nell'If-then-else per assicurarsi che i due rami restituscano dati dello stesso tipo *)
and same_type (ev1: evT) (ev2: evT) : bool =
        match (ev1, ev2) with
            | (Int(_), Int(_)) -> true
            | (Bool(_), Bool(_)) -> true
            | (String(_), String(_)) -> true
            | (Closure(_, _, _), Closure(_, _, _)) -> true
            | (RecClosure(_, _, _, _), RecClosure(_, _, _, _)) -> true
            | (Set(_, _), Set(_, _)) -> true
            | (Unbound, Unbound) -> true
            | (_, _) -> false	
(* 	Funzione ausiliaria che prende la chiusura di una funzione e la applica a un valore di default per scoprire il tipo del risultato
	Serve alla Map per capire il tipo dell'insieme da restituire (ovvero il tipo del risultato della funzione); 
	infatti in caso di applicazione su insieme vuoto, la map_aux restituisce una lista vuota di evT e non ci sono quindi elementi dai quali ricavare il tipo *)	
and find_type (fclosure: evT) (t: setType) : evT =
	match fclosure with
		| Closure(arg, fbody, fDecEnv) -> 
			(match t with
				| IntSet -> let e = Int(1) in apply_evt fclosure e
				| BoolSet -> let e = Bool(true) in apply_evt fclosure e
				| StringSet -> let e = String("test") in apply_evt fclosure e)
		| RecClosure(f, arg, fbody, fDecEnv) ->	
			(match t with
				| IntSet -> let e = Int(1) in apply_evt fclosure e
				| BoolSet -> let e = Bool(true) in apply_evt fclosure e
				| StringSet -> let e = String("test") in apply_evt fclosure e)
		| _ -> failwith("non functional value")	
;;