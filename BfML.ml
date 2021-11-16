module Exp = struct
  type op = Equals | Add | Mult | Div 
  
  type exp = 
  | Int of int
  | Bool of bool
  | If of exp * exp * exp

end

module Eval = struct
  open Exp
  exception TypeError
  let rec eval e = 
    match e with
    | Int i -> Int (i+23)
    | Bool b -> Bool (not b)
    | If (e, e1, e2) -> 
      begin match eval e with
      | Bool true -> eval e1
      | Bool false -> eval e2
      | _ -> raise TypeError
    end
end
