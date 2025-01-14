namespace Aoc.Day3

open System.Text.RegularExpressions

module Part1 =
    let getMulFromInput = "mul\(\d+,\d+\)"
    let getNumbersFromMul = "(\d+,\d+)"
    let fullRegex = "(mul\(\d*\,\d*\))|(do\(\))|(don\'t\(\))"

    let matches str = Regex.Matches(str, getMulFromInput)

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