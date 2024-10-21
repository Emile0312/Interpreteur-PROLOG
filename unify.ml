let rec unify t1 t2 = match observe t1,observe t2 with
  |Var x,Var y -> bind x (var y)
  |Fun (s,lst),Var y -> bind y (make s lst)
  |Var x,Fun (s,lst) -> bind x (make s lst)
  |Fun (s1,lst1),Fun (s2,lst2) -> if s1<>s2 then raise Unififcation_failure else begin
                                  end