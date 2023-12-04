namespace Lette.AoC2023

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day03 =

    type Cell = Empty | Digit of int | Symbol of char

    let isSymbol = function
        | Symbol _ -> true
        | _        -> false

    let isGear = function
        | Symbol '*' -> true
        | _          -> false

    let parse input () =

        let emptyP  = pchar '.'   >>% Empty
        let digitP  = digit       |>> (fun c -> Digit (int c - int '0'))
        let symbolP = noneOf "\n" |>> Symbol

        let cellP = choice [ attempt emptyP; attempt digitP; symbolP ]

        let rowP = many cellP

        let allP = sepBy rowP newlineP

        Parser.run allP input |> array2D

    let findNumbers x row =
        let rec loop cells y acc current =
            match cells, current with
            | [],            None               -> acc
            | [],            Some n             -> n :: acc
            | Digit d :: cs, None               -> loop cs (y + 1) acc        (Some (d, x, y, 1))
            | Digit d :: cs, Some (n, x, y', l) -> loop cs (y + 1) acc        (Some (n * 10 + d, x, y', l + 1))
            | _       :: cs, None               -> loop cs (y + 1) acc        None
            | _       :: cs, Some n             -> loop cs (y + 1) (n :: acc) None

        loop (row |> List.ofArray) 0 [] None
        |> List.rev

    let findConnections height width (input : Cell[,]) filter (_, x, y, l as number) =
        [
            for dx in -1 .. 1 do
            for dy in -1 .. l do
                let x' = x + dx
                let y' = y + dy
                if 0 <= x' && x' < height && 0 <= y' && y' < width then
                    if input[x', y'] |> filter then
                        Some (x', y')
                    else
                        None
                else
                    None
        ]
        |> List.choose id
        |> (fun cs -> (number, cs))

    let part1 input =

        let height = input |> Array2D.length1
        let width  = input |> Array2D.length2

        let findSymbolConnections' = findConnections height width input isSymbol

        let hasSymbolConnections number =
            number
            |> findSymbolConnections'
            |> snd
            |> List.isEmpty
            |> not

        input
        |> Array2D.mapiRows findNumbers
        |> List.concat
        |> List.filter hasSymbolConnections
        |> List.map (fun (n, _, _, _) -> n)
        |> List.sum

    let part2 input =

        let height = input |> Array2D.length1
        let width  = input |> Array2D.length2

        let findGearConnections' = findConnections height width input isGear

        let findGearConnections number =
            number
            |> findGearConnections'
            |> function
                | _, []    -> (number, None)
                | _, [ g ] -> (number, Some g)
                | _        -> failwith "more than one connected gear"

        let numberWithGearConnection (number, optionalGear) =
            match optionalGear with
            | Some gear -> Some (number, gear)
            | _         -> None

        input
        |> Array2D.mapiRows findNumbers
        |> List.concat
        |> List.map findGearConnections
        |> List.choose numberWithGearConnection
        |> List.groupBy snd
        |> List.filter (snd >> List.length >> (=) 2)
        |> List.map snd
        |> List.map (List.map (fun ((n, _, _, _), _) -> n))
        |> List.map (List.fold (*) 1)
        |> List.sum

    let puzzle =
        Puzzle.init
            3
            (getInput >> parse)
            part1 (Some 535351)
            part2 (Some 87287096)
