namespace Aoc.Day1

module Part1 =
    let removeSign a =
        match a with
        | a when a < 0 -> a * -1
        | _ -> a

    let getDifferencesBetween (list1: int list) (list2: int list) =
        List.map (fun i -> removeSign (list1[i] - list2[i])) [ 0 .. list1.Length - 1 ]

    let addThemTogether list =
        List.reduce (fun current next -> current + next) list

    let doAllThatStuff list1 list2 =
        list1 |> List.sort |> getDifferencesBetween (List.sort list2) |> addThemTogether


module Part2 =
    let numberOfOccurrencesInList (inputNumber: int) (list: int list) =
        List.fold (fun total curr -> if curr = inputNumber then total + 1 else total) 0 list

    let doAllThatStuff list1 list2 =
        let occ = List.map (fun x -> numberOfOccurrencesInList x list2) list1
        let scores = List.map (fun i -> list1[i] * occ[i]) [ 0 .. list1.Length - 1 ]

        List.reduce (fun curr next -> curr + next) scores
