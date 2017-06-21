// The second interpreter example for the first order recursive equations language.

type expr =
    | Int of int (* Constant *)
    | Var of string (* Variable *)
    | If of expr * expr * expr (* Conditional *)
    | Call of string * expr list (* Function call *)
    | Op of string * expr list (* Base application *)
let rec lookup (x, (n::ns, v::vs)) =
    if x = n then v else lookup(x, (ns, vs))
let rec eval (op, env, pgm) =
    match op with
    | Int n -> n
    | Var x -> lookup(x, env)
    | Call(f, exps) ->
        let vals = evlist(exps,env,pgm)
        let (vars, exp) = lookup(f, pgm)
        eval(exp, (vars, vals), pgm)
    | Op("+",[e1;e2]) -> (* similar for *,-,... *)
        eval(e1,env,pgm) + eval(e2,env,pgm)
    | If(e,f,g) ->
        match eval(e, env, pgm) with
        | 1 -> eval(f, env, pgm) (* 1 is true *)
        | _ -> eval(g, env, pgm) (* non-1 is false *)
and evlist (l, env, pgm) = 
    match l with 
    | [] -> []
    | e::es -> eval(e, env, pgm) :: evlist(es, env, pgm)
let interpret (pgm, args) =
    let (_, (vars, exp)::_) = pgm
    eval(exp, (vars, args), pgm)
