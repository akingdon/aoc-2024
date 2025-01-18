namespace Aoc.Day3

open System.Text.RegularExpressions

module Part1 =
    let getMulFromInput = "mul\(\d+,\d+\)"
    let getNumbersFromMul = "(\d+,\d+)"
    let fullRegex = "(?'mul'mul\(\d*\,\d*\))|(?'do'do\(\))|(?'dont'don\'t\(\))"

    let matches str pattern = Regex.Matches(str, pattern)

    let getNumbers (str: string) =
        let res = ',' |> str.Split
        let asNumbers = res |> Array.map (fun x -> x |> int32)
        asNumbers |> Array.pairwise |> Array.exactlyOne

    let multiplyThem (a, b) = a * b

    let pairs (collection: MatchCollection) =
        collection
        |> Seq.map (fun m -> Regex.Matches(m.Value, getNumbersFromMul))
        |> Seq.concat
        |> Seq.map (fun x -> x.Value |> getNumbers |> multiplyThem)
        |> Seq.reduce (fun current next -> current + next)

module Part2 =
    type Result =
        | Mul
        | Do
        | Don't

    let successfulMatch name (group: Group) = group.Name = name && group.Success

    let (|Mul|_|) (m: Match) =
        if (m.Groups |> Seq.exists (successfulMatch "mul")) then
            Some Mul
        else
            None

    let (|Do|_|) (m: Match) =
        if (m.Groups |> Seq.exists (successfulMatch "do")) then
            Some Do
        else
            None

    let (|Don't|_|) (m: Match) =
        if (m.Groups |> Seq.exists (successfulMatch "dont")) then
            Some Don't
        else
            None

    let matchType (m: Match) =
        match m with
        | Mul -> Mul
        | Do -> Do
        | _ -> Don't

    let allMatches = Part1.fullRegex |> Part1.matches Inputs.theWholeThing

    let getMatchesUntil typeToMatch =
        allMatches |> Seq.takeWhile (fun m -> m |> matchType = typeToMatch)

    let mulStringToNumbers (mulMatch: Match) =
        Regex.Match(mulMatch.Value, Part1.getNumbersFromMul).Value
        |> Part1.getNumbers
        |> Part1.multiplyThem

    let rec getTotal (coll: Match list) totalValue =
        match coll with
        | [] -> totalValue
        | head :: tail when head |> matchType = Mul -> getTotal tail (totalValue + (head |> mulStringToNumbers))
        | head :: tail when head |> matchType = Do -> getTotal tail totalValue
        | head :: tail when head |> matchType = Don't ->
            let nextMult = tail |> Seq.tryFindIndex (fun item -> item |> matchType = Do)

            if nextMult.IsSome then
                (getTotal tail[nextMult.Value ..] totalValue)
            else
                (getTotal [] totalValue)
        | _ -> totalValue
