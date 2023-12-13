namespace Lette.AoC2023

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day13 =

    let parse input () =

        let ashP = pchar '.'
        let rockP = pchar '#'
        let rowP = many1 (ashP <|> rockP) |>> String.ofList
        let patternP = sepBy' rowP newlineP |>> Array.ofList

        let allP = sepBy' patternP (newlineP .>> newlineP)

        Parser.run allP input

    let countBits maxSmudges n =
        let rec loop n acc =
            match n, acc with
            | 0, _ -> acc
            | _, s when s = (maxSmudges + 1) -> s
            | _, _ -> loop (n >>> 1) (acc + (n &&& 1))
        loop n 0

    let compareBits maxSmudges i1 i2 =
        let comparison = i1 ^^^ i2
        if comparison = 0 then
            (true, 0)
        else
            let numberOfBits = countBits maxSmudges comparison
            if numberOfBits <= maxSmudges then
                (true, numberOfBits)
            else
                (false, 0)

    let toInts pattern =
        let toInt cs =
            let folder c acc =
                (acc <<< 1) + if c = '#' then 1 else 0
            Array.foldBack folder cs 0

        let intPattern =
            pattern
            |> Array.map String.toCharArray
            |> Array.map toInt

        let width =
            pattern
            |> Array.head
            |> String.length

        (intPattern, width)

    let intToBits n i =
        let rec loop n i acc =
            match n with
            | 0 -> acc
            | _ -> loop (n - 1) (i >>> 1) ((i &&& 1) :: acc)
        loop n i [] |> List.rev |> List.toArray

    let bitsToInt bits =
        let folder acc bit = (acc <<< 1) ||| bit
        Array.fold folder 0 bits

    let transpose (pattern, width) =
        pattern
        |> Array.map (intToBits width)
        |> Array.transpose
        |> Array.map bitsToInt
        |> fun p -> (p, width)

    let findReflection expectedSmudges (pattern, _) =

        let rowCount = pattern |> Array.length

        let rec isReflectionAt smudges row1 row2 =
            if smudges > expectedSmudges then
                false
            else
                if row1 < 0 || row2 >= rowCount then
                    if smudges = expectedSmudges then
                        true
                    else
                        false
                else
                    let isMatch, smudge = compareBits (expectedSmudges - smudges) pattern[row1] pattern[row2]
                    if not isMatch then
                        false
                    else
                        isReflectionAt (smudges + smudge) (row1 - 1) (row2 + 1)

        let rec runRow row =
            if row > rowCount - 2 then
                None
            else
                let isMatch, smudge = compareBits expectedSmudges pattern[row] pattern[row + 1]
                if not isMatch then
                    runRow (row + 1)
                else
                    if isReflectionAt smudge (row - 1) (row + 2) then
                        Some (row + 1)
                    else
                        runRow (row + 1)

        runRow 0

    let runPattern expectedSmudges pattern =
        let horizontalReflection =
            pattern
            |> findReflection expectedSmudges
            |> Option.map ((*) 100)

        let verticalReflection () =
            pattern
            |> transpose
            |> findReflection expectedSmudges

        horizontalReflection
        |> Option.orElseWith verticalReflection
        |> Option.defaultWith (fun _ -> failwith "no reflections found in either direction")

    let part1 input =

        input
        |> List.map toInts
        |> List.map (runPattern 0)
        |> List.sum

    let part2 input =

        input
        |> List.map toInts
        |> List.map (runPattern 1)
        |> List.sum

    let puzzle =
        Puzzle.init
            13
            (getInput >> parse)
            part1 (Some 30158)
            part2 (Some 36474)
