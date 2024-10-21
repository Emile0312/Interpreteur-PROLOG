let rec unify t1 t2 = match observe t1,observe t2 with
  |Var x,Var y -> bind x (var y)
  |Fun (s,lst),Var y -> bind y (make s lst)
  |Var x,Fun (s,lst) -> bind x (make s lst)
  |Fun (s1,lst1),Fun (s2,lst2) -> if s1<>s2 ||then raise Unification_failure else begin
                                  let rec aux l1 l2 = match l1,l2 with
                                    |[],[] -> []
                                    |h1::q1,h2::q2 -> (unify h1 h2)::(aux q1 q2)
                                    |_ -> raise Unification_failure
                                  end;;