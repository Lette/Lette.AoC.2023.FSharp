namespace Lette.AoC2023

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day01 =

    let parse input () =

        let allP = sepBy' restOfLine1 newlineP

        Parser.run allP input

    let digits = [| '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' |]

    let findDigit indexFinder str =
        let ix = indexFinder digits str
        if ix < 0 then
            failwith "no digits found"
        else
            int (String.getChar ix str) - int '0'

    let findFirstDigit str = findDigit String.indexOfAny str

    let findLastDigit str = findDigit String.lastIndexOfAny str

    let part1 input =

        input
        |> List.map (fun s -> (findFirstDigit s, findLastDigit s))
        |> List.map (fun (a, b) -> a * 10 + b)
        |> List.sum

    let part2 input =

        let replaceWords str =
            str
            |> String.replace "one"   "o1e"
            |> String.replace "two"   "t2o"
            |> String.replace "three" "t3e"
            |> String.replace "four"  "f4r"
            |> String.replace "five"  "f5e"
            |> String.replace "six"   "s6x"
            |> String.replace "seven" "s7n"
            |> String.replace "eight" "e8t"
            |> String.replace "nine"  "n9e"

        input
        |> List.map replaceWords
        |> List.map (fun s -> (findFirstDigit s, findLastDigit s))
        |> List.map (fun (a, b) -> a * 10 + b)
        |> List.sum

    let puzzle =
        Puzzle.init
            1
            (getInput >> parse)
            part1 (Some 54239)
            part2 (Some 55343)
