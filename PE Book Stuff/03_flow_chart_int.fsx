// The third interpreter example for the flow chart language.

type expr =
    Int of int (* Integer constant *)
    | Var of string (* Variable *)
    | Op of string * expr list (* Base application *)
and command =
    Goto of int (* Goto command *)
    | Assign of string * expr (* Assignment command *)
    | If of expr * int * int (* If-then-else command *)
    | Return of expr (* Return command *)
and program = Read of string list * command list

let rec nth = function
    (c::cs, 1) -> c
    | (c::cs, n) -> nth(cs, n-1)
let rec lookup = function
    (x, ([], [])) -> 0 (* Initial value *)
    | (x, (n::ns, v::vs)) ->
        if x = n then v else lookup(x, (ns, vs))
let rec update = function
    (([], []), x, w) -> ([x], [w])
    | ((n::ns, v::vs), x, w) ->
        if x = n then n::ns, w::vs
        else 
            let (ns1, vs1) = update((ns,vs), x, w)
            n::ns1, v::vs1
let rec eval = function
    (Int n, s) -> n
    | (Var x, s) -> lookup(x, s)
    | (Op("+",[e1;e2]), s) -> eval(e1, s) + eval(e2, s)
let rec run = function
    (l, Goto n, s, p) -> run(n, nth (p, n), s, p)
    | (l, Assign(x, e), s, p) ->
        let s1 = update(s, x, eval(e, s))
        run(l+1, nth(p, l+1), s1, p)
    | (l, If(e,m,n), s, p) ->
        if eval(e, s) = 0
        then run(m, nth(p, m), s, p)
        else run(n, nth(p, n), s, p)
    | (l, Return e, s, p) -> eval(e, s)
let interpret (pgm, args) =
    let (Read (vars, cmds)) = pgm
    let (c1 :: _) = cmds
    let store = (vars, args)
    run(1, c1, store, cmds)
