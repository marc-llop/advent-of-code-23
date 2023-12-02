interface ResultExtra
    exposes [
        map2,
        sequence,
    ]
    imports []


map2 : (a, b -> c), Result a err, Result b err -> Result c err
map2 = \fn, resA, resB ->
    a <- Result.try resA
    b <- Result.map resB
    fn a b

sequence : List (Result a b) -> Result (List a) b
sequence = \list ->
    List.walk list (Ok []) (\acc, elem -> map2 List.append acc elem)
