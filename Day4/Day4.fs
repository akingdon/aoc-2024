namespace Aoc.Day4

module Part1 =

    let addTogether acc curr = acc + curr
    let isTrue x = x = true

    let checkDown (strs: string list) pos =
        strs[0].[pos] = 'X'
        && strs[1].[pos] = 'M'
        && strs[2].[pos] = 'A'
        && strs[3].[pos] = 'S'

    let checkUp (strs: string list) pos =
        strs[0].[pos] = 'S'
        && strs[1].[pos] = 'A'
        && strs[2].[pos] = 'M'
        && strs[3].[pos] = 'X'

    let checkUpsAndDowns (strs: string list) =
        [ 0 .. strs[0].Length - 1 ]
        |> List.map (fun x -> checkDown strs x || checkUp strs x)
        |> List.filter isTrue
        |> List.length

    let checkForwards (str: string) pos =
        str[pos] = 'X'
        && str[pos + 1] = 'M'
        && str[pos + 2] = 'A'
        && str[pos + 3] = 'S'

    let checkBackwards (str: string) pos =
        str[pos] = 'S'
        && str[pos + 1] = 'A'
        && str[pos + 2] = 'M'
        && str[pos + 3] = 'X'

    let checkForeAndAft (str: string) =
        [ 0 .. str.Length - 4 ]
        |> List.map (fun y -> checkForwards str y || checkBackwards str y)
        |> List.filter isTrue
        |> List.length

    let checkDiagonalNWToSE (strs: string list) pos =
        strs[0].[pos] = 'X'
        && strs[1].[pos + 1] = 'M'
        && strs[2].[pos + 2] = 'A'
        && strs[3].[pos + 3] = 'S'

    let checkDiagonalNEToSW (strs: string list) pos =
        strs[0].[pos + 3] = 'X'
        && strs[1].[pos + 2] = 'M'
        && strs[2].[pos + 1] = 'A'
        && strs[3].[pos] = 'S'

    let checkDiagonalSWToNE (strs: string list) pos =
        strs[3].[pos] = 'X'
        && strs[2].[pos + 1] = 'M'
        && strs[1].[pos + 2] = 'A'
        && strs[0].[pos + 3] = 'S'

    let checkDiagonalSEToNW (strs: string list) pos =
        strs[3].[pos + 3] = 'X'
        && strs[2].[pos + 2] = 'M'
        && strs[1].[pos + 1] = 'A'
        && strs[0].[pos] = 'S'

    let checkAllDiagonals (strs: string list) =
        [ 0 .. strs[0].Length - 4 ]
        |> List.map (fun pos ->
            [ (checkDiagonalNWToSE strs pos)
              (checkDiagonalNEToSW strs pos)
              (checkDiagonalSWToNE strs pos)
              (checkDiagonalSEToNW strs pos) ]
            |> List.filter isTrue
            |> List.length)
        |> List.reduce addTogether

    let doAFullRunOfFours (strs: string list) =
        let upsAndDowns = checkUpsAndDowns strs[0..3]
        let diagonals = checkAllDiagonals strs[0..3]

        upsAndDowns + diagonals

    let doAFullRunOfAlls (strs: string list) =
        let multipleLiners =
            [ 0 .. strs[0].Length - 4 ]
            |> List.map (fun a -> doAFullRunOfFours strs[a .. a + 3])
            |> List.reduce addTogether

        let singleLiners = strs |> List.map checkForeAndAft |> List.reduce addTogether

        multipleLiners + singleLiners

module Part2 =
    let forwards = ['M';'A';'S']
    let backwards = ['S';'A';'M']

    let oneXMAS (strs: string list) =
        ((strs[0].[0] = forwards[0] && strs[1].[1] = forwards[1] && strs[2].[2] = forwards[2]) &&
        (strs[2].[0] = forwards[0] && strs[1].[1] = forwards[1] && strs[0].[2] = forwards[2])) ||
        ((strs[0].[0] = backwards[0] && strs[1].[1] = backwards[1] && strs[2].[2] = backwards[2]) &&
        (strs[2].[0] = backwards[0] && strs[1].[1] = backwards[1] && strs[0].[2] = backwards[2])) ||
        ((strs[0].[0] = backwards[0] && strs[1].[1] = backwards[1] && strs[2].[2] = backwards[2]) &&
        (strs[2].[0] = forwards[0] && strs[1].[1] = forwards[1] && strs[0].[2] = forwards[2])) ||
        ((strs[0].[0] = forwards[0] && strs[1].[1] = forwards[1] && strs[2].[2] = forwards[2]) &&
        (strs[2].[0] = backwards[0] && strs[1].[1] = backwards[1] && strs[0].[2] = backwards[2]))
    
    let check3By3Grid (strs: string list) =
        let slidingSideways = strs[0].Length - 3
        let hits =
            [ 0 .. slidingSideways ]
            |> List.map (fun r ->
            let checks =
                [strs[0].[ r .. r + 2 ];
                strs[1].[ r .. r + 2 ];
                strs[2].[ r .. r + 2 ]]
                
            oneXMAS checks)

        hits |> List.filter Part1.isTrue |> List.length

    let stuffDoing (strs: string list) =
        let multipleLiners =
            [ 0 .. strs.Length - 3]
            |> List.map (fun z -> check3By3Grid strs[ z .. z + 2])
            |> List.reduce Part1.addTogether

        multipleLiners