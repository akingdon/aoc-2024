namespace Aoc.Day2

module Part1 =
    type Direction =
    | Increasing
    | Decreasing

    let getDirection (a, b) =
        match (a, b) with
        | a,b when a < b -> Increasing
        | _ -> Decreasing

    let (|InvalidDistance|ValidDistance|) (first, second) =
        let res = (first - second) |> abs
        if res > 3 || res < 1 then InvalidDistance else ValidDistance

    let validDistance (first, second) =
        match (first, second) with
        | ValidDistance -> true
        | InvalidDistance -> false

    let reportIsSafe report =
        let pairs = report |> List.pairwise
        let initialDirection = pairs |> List.head |> getDirection

        pairs |> List.forall (fun x ->
            x |> getDirection = initialDirection &&
            x |> validDistance)

    let doSomeStuff reports =
        reports
        |> List.filter (fun report -> report |> reportIsSafe)
        |> List.length