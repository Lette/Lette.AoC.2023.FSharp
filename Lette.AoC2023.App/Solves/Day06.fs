namespace Lette.AoC2023

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day06 =

    let parse input () =

        let timesP = pstring "Time:" >>. many1 spaceP >>. sepBy' pint64 (many1 spaceP)
        let distancesP = pstring "Distance:" >>. many1 spaceP >>. sepBy' pint64 (many1 spaceP)

        let allP = timesP .>> newlineP .>>. distancesP |>> flip (||>) List.zip

        Parser.run allP input

    let part1 input =

        let runRace totalTime =
            let boosts = [ 0L .. totalTime ]

            let findDistance totalTime boost =
                let remainingTime = totalTime - boost
                boost * remainingTime

            boosts
            |> List.map (findDistance totalTime)

        let toNumberOfWins (d, results) =
            results
            |> List.filter ((<) d)
            |> List.length

        input
        |> List.map (fun (t, d) -> (d, runRace t))
        |> List.map toNumberOfWins
        |> List.fold (*) 1

    let part2 input =

        let antiKerning races =
            let antiKerning' (acct, accd) (t, d) =
                (acct + string t, accd + string d)

            races
            |> List.fold antiKerning' ("", "")
            |> fun (t, d) -> (int64 t, int64 d)

        let time, distance = input |> antiKerning

        // x * (t - x) = d
        // tx - x^2 = d
        // x^2 - tx + d = 0

        // x = (-p +- sqrt(p^2 - 4q)) / 2

        // p = -t
        // q = d

        // x = (t +- sqrt(t^2 - 4d)) / 2

        let r = time * time - 4L * distance
        let sqrtr = sqrt (float r)

        let x1 = (float time - sqrtr) / 2.
        let x2 = (float time + sqrtr) / 2.

        let x1' = ceil x1 |> int64
        let x2' = floor x2 |> int64

        x2' - x1' + 1L

    let puzzle =
        Puzzle.init
            6
            (getInput >> parse)
            part1 (Some 1155175)
            part2 (Some 35961505L)
