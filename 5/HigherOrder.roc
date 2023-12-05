interface HigherOrder
    exposes [
        identity,
        log,
        cond,
        apply,
        compose,
        chain,
    ]
    imports []


identity : a -> a
identity = \a -> a

log : a -> a
log = \a ->
    dbg a
    a

cond : (a -> Bool), (a -> b), (a -> b) -> (a -> b)
cond = \predicate, thenFn, elseFn ->
    \a ->
        if predicate a
            then thenFn a
            else elseFn a

apply : (a -> b), a -> b
apply = \fn, a -> fn a

compose : (a -> b), (b -> c) -> (a -> c)
compose = \fnA, fnB ->
    \a -> fnB (fnA a)

chain : List (a -> a) -> (a -> a)
chain = \fns ->
    List.walk fns identity compose
