namespace Lette.AoC2023

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day02 =

    type Cube = Red | Green | Blue

    type Hand = Hand of (Cube * int) list

    type Game = Game of int * Hand list

    let parse input () =

        let redsP   = pint32 .>> spaceP .>> pstring "red"   |>> fun x -> (Red, x)
        let greensP = pint32 .>> spaceP .>> pstring "green" |>> fun x -> (Green, x)
        let bluesP  = pint32 .>> spaceP .>> pstring "blue"  |>> fun x -> (Blue, x)

        let partP = choice [attempt redsP; attempt greensP; bluesP]
        let handP = sepBy partP (pstring ", ") |>> Hand
        let handsP = sepBy handP (pstring "; ")

        let gameP = pstring "Game " >>. pint32 .>> pstring ": " .>>. handsP |>> Game

        let allP = sepBy' gameP newlineP

        Parser.run allP input

    let part1 input =

        let rec isValidHand (Hand parts) =
            match parts with
            | [] -> true
            | (Red,   r) :: ps when r <= 12 -> isValidHand (Hand ps)
            | (Green, g) :: ps when g <= 13 -> isValidHand (Hand ps)
            | (Blue,  b) :: ps when b <= 14 -> isValidHand (Hand ps)
            | _ -> false

        let isValidGame (Game (_, hands)) =
            hands |> List.forall isValidHand

        input
        |> List.filter isValidGame
        |> List.map (fun (Game (id, _)) -> id)
        |> List.sum

    let part2 input =

        let findMinimumNeededCubes (Game (_, hands)) =

            let foldHand (r, g, b) (c, n) =
                match c with
                | Red   -> (max r n, g, b)
                | Green -> (r, max g n, b)
                | Blue  -> (r, g, max b n)

            let foldHands (r, g, b) (Hand parts) =
                parts
                |> List.fold foldHand (r, g, b)

            hands
            |> List.fold foldHands (0, 0, 0)

        let findPower (r, g, b) = r * g * b

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
