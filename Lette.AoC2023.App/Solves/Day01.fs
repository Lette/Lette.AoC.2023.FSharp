namespace Lette.AoC2023

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day01 =

    let parse input () =

        let allP = sepBy' (opt spaceP) newlineP

        Parser.run allP input

    let part1 input =

        0

    let part2 input =

        0

    let puzzle =
        Puzzle.init
            1
            (parse (getInput 1))
            part1 None
            part2 None
