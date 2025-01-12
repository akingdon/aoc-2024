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
    let getAllPermutations list =
        let length = list |> List.length
        let indexer = [0 .. (length - 1)]
        let perms = indexer |> List.map (fun i -> (list |> (List.removeAt i)))
        perms

    let anyPermWorks list =
        let possibleOnes = list |> getAllPermutations
        let succeeded = possibleOnes |> List.filter (fun l -> l |> Part1.reportIsSafe)
        succeeded |> List.length > 0

    let doSomeStuff reports =
        let failingReports = reports |> List.filter (fun r -> Part1.reportIsSafe r <> true)

        failingReports
        |> List.filter (fun r -> r |> anyPermWorks)
        |> List.length