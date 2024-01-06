namespace Lette.AoC2023

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module DayXX =

    let parse input () =

        let allP = sepBy' (opt spaceP) newlineP

        Parser.run allP input

    let part1 input =

        0

    let part2 input =

        0

    let puzzle =
        Puzzle.init
            0
            (parse (getInput 0))
            part1 None
            part2 None
