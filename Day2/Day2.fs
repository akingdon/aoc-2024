namespace Aoc.Day2

module Part1 =
    type Direction =
    | Increasing
    | Decreasing

    type Distance =
    | TooGreat
    | Acceptable
    | Equal

    let getDirection (a, b) =
        match (a, b) with
        | a,b when a < b -> Increasing
        | _ -> Decreasing

    let getDistance (a, b) =
        let distance = a - b |> abs

        match distance with
        | distance when distance > 3 -> TooGreat
        | distance when distance > 0 -> Acceptable
        | _ -> Equal

    let initialDirection list = list |> List.head |> getDirection

    let validDistance (a, b) =
        match (a, b) |> getDistance with
        | TooGreat | Equal -> false
        | _ -> true

    let allDistancesAreValid list =
        list |> List.forall (fun x -> x |> validDistance)

    let allDirectionsMatch list =
        let firstDirection = list |> initialDirection
        list |> List.tail |> List.forall (fun x -> x |> getDirection = firstDirection)

    let reportIsSafe report =
        let pairs = report |> List.pairwise

        pairs |> allDistancesAreValid && pairs |> allDirectionsMatch

    let doSomeStuff reports =
        reports
        |> List.filter (fun report -> report |> reportIsSafe)
        |> List.length

module Part2 =
    let getEqualDistancePair list =
        list |> List.filter (fun x -> x |> Part1.getDistance = Part1.Equal)

    let reversePairwise list =
        list |> List.map (fun x -> x |> fst)

    let reportIsSafeWithProblemDampener report =
        let pairs = report |> List.pairwise
        let firstDirection = pairs |> Part1.initialDirection

        let directionErrorsRemoved = pairs |> List.filter (fun x -> x |> Part1.getDirection = firstDirection)
        let distanceErrorsRemoved = directionErrorsRemoved |> List.filter (fun x -> x |> Part1.validDistance)

        let newReport = distanceErrorsRemoved |> reversePairwise
        let newReportPasses = newReport |> Part1.reportIsSafe
        let newReportHadOneErrorRemoved = (newReport |> List.length) - (report |> List.length) |> abs = 1

        newReportPasses && newReportHadOneErrorRemoved

    let doSomeStuff reports =
        reports
        |> List.filter (fun report -> report |> reportIsSafeWithProblemDampener)
        |> List.length