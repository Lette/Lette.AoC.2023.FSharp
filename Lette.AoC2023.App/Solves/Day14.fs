namespace Lette.AoC2023

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day14 =

    type Cell =
        | RoundedRock
        | CubeShapedRock
        | Empty

    let parse input () =

        let roundedRockP = pchar 'O' >>% RoundedRock
        let cubeShapedRockP = pchar '#' >>% CubeShapedRock
        let emptyP = pchar '.' >>% Empty

        let rowP = many (roundedRockP <|> cubeShapedRockP <|> emptyP)

        let allP = sepBy' rowP newlineP

        Parser.run allP input
        |> array2D

    let tiltNorth (input : Cell array2d) sizeX sizeY () =

        let rollRock x y =
            let rec findNewSpot x =
                if x = 0 then
                    0
                else
                    if (input[x - 1, y] = Empty) then
                        findNewSpot (x - 1)
                    else
                        x
            let x' = findNewSpot x

            if x' <> x then
                input[x, y] <- Empty
                input[x', y] <- RoundedRock

            ()

        for x in 1 .. (sizeX - 1) do
            for y in 0 .. sizeY - 1 do
                if input[x, y] = RoundedRock then
                    rollRock x y

        ()

    let tiltWest (input : Cell array2d) sizeX sizeY () =

        let rollRock x y =
            let rec findNewSpot y =
                if y = 0 then
                    0
                else
                    if (input[x, y - 1] = Empty) then
                        findNewSpot (y - 1)
                    else
                        y
            let y' = findNewSpot y

            if y' <> y then
                input[x, y] <- Empty
                input[x, y'] <- RoundedRock

            ()

        for y in 1 .. (sizeY - 1) do
            for x in 0 .. sizeX - 1 do
                if input[x, y] = RoundedRock then
                    rollRock x y

        ()

    let tiltSouth (input : Cell array2d) sizeX sizeY () =

        let rollRock x y =
            let rec findNewSpot x =
                if x = (sizeX - 1) then
                    sizeX - 1
                else
                    if (input[x + 1, y] = Empty) then
                        findNewSpot (x + 1)
                    else
                        x
            let x' = findNewSpot x

            if x' <> x then
                input[x, y] <- Empty
                input[x', y] <- RoundedRock

            ()

        for x in (sizeX - 2) .. -1 .. 0 do
            for y in 0 .. sizeY - 1 do
                if input[x, y] = RoundedRock then
                    rollRock x y

        ()

    let tiltEast (input : Cell array2d) sizeX sizeY () =

        let rollRock x y =
            let rec findNewSpot y =
                if y = (sizeY - 1) then
                    sizeY - 1
                else
                    if (input[x, y + 1] = Empty) then
                        findNewSpot (y + 1)
                    else
                        y
            let y' = findNewSpot y

            if y' <> y then
                input[x, y] <- Empty
                input[x, y'] <- RoundedRock

            ()

        for y in (sizeY - 2) .. -1 .. 0 do
            for x in 0 .. sizeX - 1 do
                if input[x, y] = RoundedRock then
                    rollRock x y

        ()

    let tilt input sizeX sizeY () =
        tiltNorth input sizeX sizeY ()
        tiltWest input sizeX sizeY ()
        tiltSouth input sizeX sizeY ()
        tiltEast input sizeX sizeY ()

    let countWeights input sizeX () =
        let weightFolder acc x _ c =
            match c with
            | RoundedRock -> (sizeX - x) + acc
            | _           -> acc
        Array2D.foldi weightFolder 0 input

    let tryFindCycleLength xs =
        let rec loop xs map cycleLength lastCycleLength i =
            match xs with
            | []       -> cycleLength
            | x :: xs' ->
                if Map.containsKey x map then
                    let newCycleLength = Some (i - Map.find x map)
                    if lastCycleLength = newCycleLength then
                        loop xs' (Map.add x i map) newCycleLength lastCycleLength (i + 1)
                    else
                        loop xs' (Map.add x i map) cycleLength newCycleLength (i + 1)
                else
                    loop xs' (Map.add x i map) cycleLength lastCycleLength (i + 1)

        loop xs Map.empty None None 0

    let findCycleLength xs =
        match tryFindCycleLength xs with
        | None   -> failwith "no cycle found"
        | Some x -> x

    let part1 input =

        let sizeX = input |> Array2D.length1
        let sizeY = input |> Array2D.length2

        tiltNorth input sizeX sizeY ()
        countWeights input sizeX ()

    let part2 cycles input =

        let sizeX = input |> Array2D.length1
        let sizeY = input |> Array2D.length2

        let preTilts = 200
        let testTilts = 50

        for _ in 1 .. preTilts do
            tilt input sizeX sizeY ()

        let tilt' = tilt input sizeX sizeY
        let countWeights' = countWeights input sizeX

        let weights =
            [ (preTilts + 1) .. (preTilts + testTilts) ]
            |> List.map (fun i -> tilt' (); (countWeights' (), i))

        let cycleLength = findCycleLength (weights |> List.map fst)
        let targetCycleMod = cycles % cycleLength
        let firstMod = (preTilts + 1) % cycleLength

        let targetIx = (preTilts + 1) + ((targetCycleMod + cycleLength - firstMod) % cycleLength)

        weights
        |> List.find (snd >> (=) targetIx)
        |> fst

    let puzzle =
        Puzzle.init
            14
            (getInput >> parse)
            part1 (Some 109665)
            (part2 1000000000) (Some 96061)
