namespace Lette.AoC2023

open Xunit
open FsUnit.Xunit

module Day01Tests =

    open Day01

    let input = "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"

    [<Fact>]
    let ``Parser works`` () =

        let result = parse input ()

        result |> should equal
            [
                "1abc2"
                "pqr3stu8vwx"
                "a1b2c3d4e5f"
                "treb7uchet"
            ]

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 142

    [<Fact>]
    let ``Part 2`` () =
        let input = "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"

        let result = part2 (parse input ())

        result |> should equal 281
