namespace Lette.AoC2023

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day02 =

    type Cube = Red | Green | Blue
    type Hand = Hand of (Cube * int) list
    type Game = Game of int * Hand list

    let parse input () =

        let redsP   = pint32 .>> spaceP .>> pstring "red"   |>> fun x -> (Red,   x)
        let greensP = pint32 .>> spaceP .>> pstring "green" |>> fun x -> (Green, x)
        let bluesP  = pint32 .>> spaceP .>> pstring "blue"  |>> fun x -> (Blue,  x)

        let partP = choice [attempt redsP; attempt greensP; bluesP]
        let handP = sepBy partP (pstring ", ") |>> Hand
        let handsP = sepBy handP (pstring "; ")

        let gameNumberP = pstring "Game " >>. pint32 .>> pstring ": "
        let gameP = gameNumberP .>>. handsP |>> Game

        let allP = sepBy' gameP newlineP

        Parser.run allP input

    let part1 input =

        let rec areValidParts parts =
            match parts with
            | [] -> true
            | (Red,   r) :: ps when r <= 12 -> areValidParts ps
            | (Green, g) :: ps when g <= 13 -> areValidParts ps
            | (Blue,  b) :: ps when b <= 14 -> areValidParts ps
            | _ -> false

        let isValidHand (Hand parts) =
            areValidParts parts

        let isValidGame (Game (_, hands)) =
            hands |> List.forall isValidHand

        input
        |> List.filter isValidGame
        |> List.map (fun (Game (id, _)) -> id)
        |> List.sum

    let part2 input =

        let findMinimumNeededCubes (Game (_, hands)) =

            let foldPart (r, g, b) (cube, amount) =
                match cube with
                | Red   -> (max r amount, g,            b           )
                | Green -> (r,            max g amount, b           )
                | Blue  -> (r,            g,            max b amount)

            let foldHand (r, g, b) (Hand parts) =
                parts
                |> List.fold foldPart (r, g, b)

            hands
            |> List.fold foldHand (0, 0, 0)

        let findPower (reds, greens, blues) = reds * greens * blues

        input
        |> List.map findMinimumNeededCubes
        |> List.map findPower
        |> List.sum

    let puzzle =
        Puzzle.init
            2
            (getInput >> parse)
            part1 (Some 3035)
            part2 (Some 66027)
