open Aoc

let d1p1 = Day1.Part1.getTotalDistance Day1.Inputs.list1 Day1.Inputs.list2
printfn $"Day 1 Part 1 result: {d1p1}"

let d1p2 = Day1.Part2.doAllThatStuff Day1.Inputs.list1 Day1.Inputs.list2
printfn $"Day 1 Part 2 result: {d1p2}"


let d2p1 = Day2.Part1.doSomeStuff Day2.Inputs.listOfLists
printfn $"Day 2 Part 1 result: {d2p1}"