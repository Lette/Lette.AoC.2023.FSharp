namespace Lette.AoC2023

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day07 =

    let toCardValue = function
        | 'A' -> 14
        | 'K' -> 13
        | 'Q' -> 12
        | 'J' -> 11
        | 'T' -> 10
        | c   -> int c - int '0'

    let parse input () =

        let cardP = noneOf " " |>> toCardValue
        let handP = many cardP
        let rowP = handP .>> spaceP .>>. pint32

        let allP = sepBy' rowP newlineP

        Parser.run allP input

    let highCard = 1
    let onePair = 2
    let twoPairs = 3
    let threeOfAKind = 4
    let fullHouse = 5
    let fourOfAKind = 6
    let fiveOfAKind = 7

    let handTypeValue evaluator hand =
        hand
        |> List.groupBy id
        |> List.map (fun (k, v) -> (k, List.length v))
        |> List.sortByDescending (fun (c, n) -> 100 * n + c)
        |> evaluator

    let compareHands evaluator h1 h2 =
        let handTypeValue' = handTypeValue evaluator
        let handTypeValueComparison = (handTypeValue' h1) - (handTypeValue' h2)
        if handTypeValueComparison <> 0 then
            handTypeValueComparison
        else
            let rec compareCards cards =
                match cards with
                | [] -> 0
                | (c1, c2) :: cs when c1 = c2 -> compareCards cs
                | (c1, c2) :: _               -> c1 - c2

            compareCards (List.zip h1 h2)

    let compareRows evaluator (h1, _) (h2, _) = compareHands evaluator h1 h2

    let toWinning rank (_, bid) = rank * bid

    let part1 input =

        let evaluator groupedHand =
            groupedHand
            |> List.map snd
            |> function
                | 5 ::                [] -> fiveOfAKind
                | 4 :: 1 ::           [] -> fourOfAKind
                | 3 :: 2 ::           [] -> fullHouse
                | 3 :: 1 :: 1 ::      [] -> threeOfAKind
                | 2 :: 2 :: 1 ::      [] -> twoPairs
                | 2 :: 1 :: 1 :: 1 :: [] -> onePair
                | _                      -> highCard

        let compareRows' = compareRows evaluator

        input
        |> List.sortWith compareRows'
        |> List.mapi (((+) 1) >> toWinning)
        |> List.sum

    let part2 input =

        let evaluator =
            function
            | (_, 5) :: [] -> fiveOfAKind

            | (1, 4) :: (_, 1) :: [] -> fiveOfAKind
            | (_, 4) :: (1, 1) :: [] -> fiveOfAKind
            | (_, 4) :: (_, 1) :: [] -> fourOfAKind

            | (1, 3) :: (_, 2) :: [] -> fiveOfAKind
            | (_, 3) :: (1, 2) :: [] -> fiveOfAKind
            | (_, 3) :: (_, 2) :: [] -> fullHouse

            | (1, 3) :: (_, 1) :: (_, 1) :: [] -> fourOfAKind
            | (_, 3) :: (_, 1) :: (1, 1) :: [] -> fourOfAKind
            | (_, 3) :: (_, 1) :: (_, 1) :: [] -> threeOfAKind

            | (_, 2) :: (1, 2) :: (_, 1) :: [] -> fourOfAKind
            | (_, 2) :: (_, 2) :: (1, 1) :: [] -> fullHouse
            | (_, 2) :: (_, 2) :: (_, 1) :: [] -> twoPairs

            | (1, 2) :: (_, 1) :: (_, 1) :: (_, 1) :: [] -> threeOfAKind
            | (_, 2) :: (_, 1) :: (_, 1) :: (1, 1) :: [] -> threeOfAKind
            | (_, 2) :: (_, 1) :: (_, 1) :: (_, 1) :: [] -> onePair

            | (_, 1) :: (_, 1) :: (_, 1) :: (_, 1) :: (1, 1) :: [] -> onePair
            | _                                                    -> highCard

        let remapJacks (hand, bid) =
            let hand' = hand |> List.map (fun c -> if c = 11 then 1 else c)
            (hand', bid)

        let compareRows' = compareRows evaluator

        input
        |> List.map remapJacks
        |> List.sortWith compareRows'
        |> List.mapi (((+) 1) >> toWinning)
        |> List.sum

    let puzzle =
        Puzzle.init
            7
            (getInput >> parse)
            part1 (Some 249204891)
            part2 (Some 249666369)
