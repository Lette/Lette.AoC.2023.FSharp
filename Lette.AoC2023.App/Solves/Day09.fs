namespace Lette.AoC2023

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day09 =

    let parse input () =

        let rowP = sepBy' pint32 spaceP
        let allP = sepBy' rowP newlineP

        Parser.run allP input

    let (|IsAllZeros|_|) xs =
        if List.forall ((=) 0) xs then Some () else None

    let runRow row =

        let processRow row =
            let rec loop row acc =
                match row with
                | a :: b :: rs -> loop (b :: rs) (b - a :: acc)
                | _ -> acc
            loop row []
            |> List.rev

        let rec loop row acc =
            match row with
            | IsAllZeros -> acc
            | _ ->
                let nextRow = processRow row
                loop nextRow (nextRow :: acc)

        loop row [row]

    let part1 input =

        let findNext rows =
            rows |> List.map List.last |> List.sum

        input
        |> List.map (runRow >> findNext)
        |> List.sum

    let part2 input =

        let findPrevious rows =
            rows |> List.map List.head |> List.fold (fun acc a -> a - acc ) 0

        input
        |> List.map (runRow >> findPrevious)
        |> List.sum

    let puzzle =
        Puzzle.init
            9
            (getInput >> parse)
            part1 (Some 1834108701)
            part2 (Some 993)
