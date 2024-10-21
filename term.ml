
(*pour le moment*)
type t = Fun_t of string * t list | Var_t of var 
type var = string 

(*ne va pas changer*)
type obs_t = Fun of string * t list | Var of var


let observe (terme : t) : obs_t = 
	match terme with 
	| Fun_t (s,l) -> Fun(s,l)
	| Var_t (s) -> !etats (Var (s))

let make (function_name : string) (variables : t list) : t = 
	Fun_t(function_name, variables)

let var (variable_name : var) : t = 
	Var_t(variable_name)

let free_name = ref ""

let fresh () = 
	free_name := !free_name ^ "free_name";
	while not (equals (use (var !free_name equal)) ((Var_t (!free_name)))) do 
		free_name := !free_name ^ "free_name";
	done;
	!free_name

let fresh_var () = () |> fresh |> var 

let var_equals (v1 : var) (v2 : var) : bool = (v1 = v2)

let list_fold_two_fun (f : 'a -> 'a -> 'b -> 'b) (l1 : 'a list) (l2 : 'a list) 

let equals (t1 : var) (t2 : var) : bool = 
	match t1,t2 with 
	| Fun_t (s1, l1), Fun_t (s2,l2) when s1 = s2 ->



 


