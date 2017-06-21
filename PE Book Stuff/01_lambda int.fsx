// The interpreter from page 51 for the untyped lambda calculus language.

type lambda =
    | Int of int                        (* Constant *)
    | Var of string                     (* Variable *)
    | Abs of string * lambda            (* Abstraction *)
    | Apply of lambda * lambda          (* Application *)
    | Op of string * lambda list        (* Base application *)
    | If of lambda * lambda * lambda    (* Conditional *)
and value =
    | Numb of int
    | Closure of lambda * (string list * value list)

let rec lookup (x, (n::ns, v::vs)) = 
    if n = x then v else lookup(x, (ns, vs))

let rec eval (op, env) = 
    match op with
    | Int n -> Numb n
    | Var x -> lookup(x, env)
    | Abs(x,e) -> Closure(Abs(x,e), env)
    | Apply(e,f) ->
        let f1 = eval(f, env)
        let (Closure(Abs(x,e1), (ns, vs))) = eval(e, env)
        eval (e1, (x::ns, f1::vs))
    | Op("+",[e1; e2]) ->
        let (Numb v1) = eval (e1, env)
        let (Numb v2) = eval (e2, env)
        Numb (v1 + v2)
    | If (e,f,g) ->
        match eval(e, env) with
        |(Numb 1) -> eval(f, env)   (* 1 is true *)
        | (Numb _) -> eval(g, env)  (* non-1 is false *)
let rec interpret e = eval (e, ([], []))