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

  (*Evaluates 4 BfML primitive operators*)
  let evalOp (po : op * exp list) : exp option = 
    match po with
    | (Equals ,[Int i1 ; Int i2;]) -> Some (Bool (i1 = i2))
    | (Add, [Int i1 ; Int i2;]) -> Some (Int (i1 + i2))
    | (Mult, [Int i1; Int i2;]) -> Some (Int (i1 * i2))
    | (Div, [Int i1; Int i2;]) -> Some (Int (i1 / i2))
    | _ -> None (*For now using Option/None to represent invalid arguments*)
    (*TODO: Change to use an InvalidArgumentsExcpetion*)
  ;;
  (*Evaluates expressions*)
  let rec eval (e : exp) : exp = 
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
