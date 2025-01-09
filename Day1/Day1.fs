namespace Aoc.Day1

module Part1 =
    let getTotalDistance list1 list2 =
        list2
        |> List.sort
        |> List.zip (List.sort list1)
        |> List.map (fun (item1,item2) -> item1 - item2 |> abs)
        |> List.reduce (fun curr next -> curr + next)

module Part2 =
    let (|MatchingPair|NoMatchingPair|) (left, right) =
        if left = right then MatchingPair else NoMatchingPair

    let getIncrement (left, right) =
        match (left, right) with
        | MatchingPair -> 1
        | NoMatchingPair -> 0

    let numberOfOccurrencesInList (inputNumber: int) (list: int list) =
        List.fold (fun total curr ->
            (inputNumber, curr)
            |> getIncrement
            |> (+) total) 0 list

    let doAllThatStuff (list1: int list) list2 =
        list1
        |> List.map (fun item -> numberOfOccurrencesInList item list2)
        |> List.map2 (fun i i2 -> i * i2) list1
        |> List.reduce (fun curr next -> curr + next)