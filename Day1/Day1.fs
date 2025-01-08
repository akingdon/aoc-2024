namespace Aoc.Day1

module Part1 =
    let removeSign a b =
        match a with
        | a when a - b < 0 -> (a - b) * -1
        | _ -> a - b
    
    let getDifferencesBetween (list1: int list) (list2: int list) =
        List.map2 (fun item item2 -> removeSign item item2) list1 list2

    let addThemTogether list =
        List.reduce (fun current next -> current + next) list

    let doAllThatStuff list1 list2 =
        list1 |> List.sort |> getDifferencesBetween (List.sort list2) |> addThemTogether

module Part2 =
    let numberOfOccurrencesInList (inputNumber: int) (list: int list) =
        List.fold (fun total curr -> if curr = inputNumber then total + 1 else total) 0 list

    let doAllThatStuff list1 list2 =
        let occurrences = List.map (fun i -> numberOfOccurrencesInList i list2) list1
        let scores = List.map2 (fun i i2 -> i * i2) list1 occurrences

        List.reduce (fun curr next -> curr + next) scores
