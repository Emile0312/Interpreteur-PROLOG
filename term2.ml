(** Le type [t] correspond à la représentation interne des termes.
  * Le type [var] représente les variables, c'est à dire les objets que
  * l'on peut instantier.
  * Le type [obs_t] correspond à un terme superficiellement explicité. *)

  type t = Fun_t of string * t list | Var_t of var
  type var = string
  type obs_t = Fun of string * t list | Var of var

  type state = var -> t

  let (current_state : state) = ref (fun x : var -> Var_t(x)) 

(** Modification d'une variable. *)
val bind : var -> t -> unit
let bind (x : var) (terme : t) = current_state := (fun y -> if(y = x) then t else (!current_state y))

(** [save ()] renvoie un descripteur de l'état actuel. *)
val save : unit -> state
let save () = !current_state;

(** [restore s] restaure les variables dans l'état décrit par [s]. *)
val restore : state -> unit
let restore (ancien_state : state) = current_state := ancien_state

(** Remise à zéro de l'état interne du module.
    Garantit que les futurs usages seront comme 
    dans un module fraichement initialisé. *)
val reset : unit -> unit
let reset () = current_state := ref (fun x : var -> Var_t(x)) 
