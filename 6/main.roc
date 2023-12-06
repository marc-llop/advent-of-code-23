app "hello"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [cli.Stdout]
    provides [main] to cli

Race : {maxTime: Nat, record: Nat}

timeToDistance : Nat, Nat -> Nat
timeToDistance = \pressingTime, maxTime ->
    travellingTime = (maxTime - pressingTime)
    travellingSpeed = pressingTime
    travellingSpeed * travellingTime

beatsRecord : Nat, Race -> Bool
beatsRecord = \pressingTime, {maxTime, record} ->
    timeToDistance pressingTime maxTime > record

find : Nat, Nat, (Nat -> Bool) -> Nat
find = \minTime, minTimeToBeatTheRecord, predicate ->
    chosenTime = (minTime + minTimeToBeatTheRecord) // 2
    dbg (chosenTime, (minTime, minTimeToBeatTheRecord))
    if chosenTime == minTime
        then minTimeToBeatTheRecord
        else if chosenTime == minTimeToBeatTheRecord
            then chosenTime
            else if predicate chosenTime
                then find minTime chosenTime predicate
                else find chosenTime minTimeToBeatTheRecord predicate

findFirst : Race -> Nat
findFirst = \race ->
    find 0 race.maxTime \time -> beatsRecord time race

findLast : Race -> Nat
findLast = \race ->
    find 0 race.maxTime \time -> !(beatsRecord time race)

differentWaysToWin : Race -> Nat
differentWaysToWin = \race ->
    (findLast race) - (findFirst race)

# Time:        44707080
# Distance:   283113411341491

main =
    result = differentWaysToWin {maxTime: 44707080, record: 283113411341491}
    Stdout.line (Num.toStr result)