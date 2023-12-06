app "hello"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [cli.Stdout]
    provides [main] to cli

timeToDistance : Nat, Nat -> Nat
timeToDistance = \pressingTime, maxTime ->
    travellingTime = (maxTime - pressingTime)
    travellingSpeed = pressingTime
    travellingSpeed * travellingTime

beatsRecord : Nat, Nat, Nat -> Bool
beatsRecord = \pressingTime, maxTime, record ->
    timeToDistance pressingTime maxTime > record

findFirst : Nat, Nat, Nat, Nat -> Nat
findFirst = \minTime, minTimeToBeatTheRecord, maxTime, record ->
    chosenTime = (minTime + minTimeToBeatTheRecord) // 2
    dbg (chosenTime, (minTime, minTimeToBeatTheRecord))
    if chosenTime == minTime
        then minTimeToBeatTheRecord
        else if chosenTime == minTimeToBeatTheRecord
            then chosenTime
            else if beatsRecord chosenTime maxTime record
                then findFirst minTime chosenTime maxTime record
                else findFirst chosenTime minTimeToBeatTheRecord maxTime record

main =
    first = findFirst 0 44 44 283
    Stdout.line (Num.toStr first)