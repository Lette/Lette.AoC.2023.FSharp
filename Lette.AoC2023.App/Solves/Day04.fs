namespace Lette.AoC2023

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day04 =

    type CardNumber = CardNumber of int
    type Winning = Winning of int list
    type Played = Played of int list

    type Card = Card of CardNumber * Winning * Played

    let parse input () =

        let singleDigitP = spaceP >>. pint32
        let doubleDigitP = pint32
        let numberP = (attempt singleDigitP <|> doubleDigitP)

        let numbersP = sepBy' numberP spaceP

        let numberSetsP =
            numbersP .>> pstring " | " .>>. numbersP
            |>> (fun (w, p) -> (Winning w, Played p))

        let cardNumberP =
            pstring "Card" >>. skipMany spaceP >>. pint32
            |>> CardNumber

        let cardP =
            cardNumberP .>> pstring ": " .>>. numberSetsP
            |>> (fun (n, (w, p)) -> Card (n, w, p))

        let allP = sepBy' cardP newlineP

        Parser.run allP input

    let numberOfWinningNumbersPlayed (Card (cardNumber, Winning winning, Played played)) =
        [winning; played]
        |> List.concat
        |> List.groupBy id
        |> List.map (snd >> List.length)
        |> List.filter ((=) 2)
        |> List.length
        |> fun length -> (cardNumber, length)

    let part1 input =

        let worth n =
            if n = 0 then
                0
            else
                pown 2 (n - 1)

        input
        |> List.map numberOfWinningNumbersPlayed
        |> List.map (snd >> worth)
        |> List.sum

    let part2 input =

        let totalCards = input |> List.length

        let cardsWon (CardNumber c, winningNumbersCount) =
            let wonCards =
                [ (c + 1) .. (min totalCards (c + winningNumbersCount)) ]
                |> List.map CardNumber
            (CardNumber c, wonCards)

        let winnings =
            input
            |> List.map numberOfWinningNumbersPlayed
            |> List.map cardsWon

        let mapOfWorth =
            let rec totalWorthOfWonCards wonCards accMap accWorth =
                match wonCards with
                | []      -> accWorth
                | w :: ws -> totalWorthOfWonCards ws accMap (accWorth + (Map.find w accMap))

            let rec findWorth winnings accMap =
                match winnings with
                | []            -> accMap
                | (c, ws) :: cs -> findWorth cs (Map.add c (1 + totalWorthOfWonCards ws accMap 0) accMap)

            findWorth (winnings |> List.rev) Map.empty

        mapOfWorth
        |> Map.values
        |> Seq.sum

    let puzzle =
        Puzzle.init
            4
            (getInput >> parse)
            part1 (Some 23673)
            part2 (Some 12263631)
