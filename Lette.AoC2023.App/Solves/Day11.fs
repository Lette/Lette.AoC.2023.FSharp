namespace Lette.AoC2023

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day11 =

    type Cell =
        | Empty
        | Galaxy

    let parse input () =

        let emptyP  = pchar '.' >>% Empty
        let galaxyP = pchar '#' >>% Galaxy

        let rowP = many (emptyP <|> galaxyP)

        let allP = sepBy' rowP newlineP

        Parser.run allP input

    let (|AreAllEmpty|_|) xs =
        if List.forall ((=) Empty) xs then Some () else None

    let findGalaxies space =

        let rowFolder x acc y cell = if cell = Galaxy then (x, y) :: acc else acc
        let spaceFolder acc x row = row |> List.foldi (rowFolder x) acc

        space
        |> List.foldi spaceFolder []

    let distanceWithExpansion space expansionFactor =

        let rowMapper row =
            match row with
            | AreAllEmpty -> expansionFactor
            | _           -> 1L

        let verticalExpansion =
            space
            |> List.transpose
            |> List.map rowMapper
            |> List.toArray

        let horizontalExpansion =
            space
            |> List.map rowMapper
            |> List.toArray

        let distance (x1, y1) (x2, y2) =
            let minX = min x1 x2
            let maxX = max x1 x2
            let minY = min y1 y2
            let maxY = max y1 y2

            let dx = horizontalExpansion[minX .. maxX - 1] |> Array.sum
            let dy = verticalExpansion[minY .. maxY - 1] |> Array.sum

            dx + dy

        distance

    let part1 input =

        let distance = distanceWithExpansion input 2L

        input
        |> findGalaxies
        |> List.pairs
        |> List.map (flip (||>) distance)
        |> List.sum

    let part2 expansionFactor input =

        let distance = distanceWithExpansion input expansionFactor

        input
        |> findGalaxies
        |> List.pairs
        |> List.map (flip (||>) distance)
        |> List.sum

    let puzzle =
        Puzzle.init
            11
            (getInput >> parse)
            part1 (Some 9312968L)
            (part2 1000000L) (Some 597714117556L)
