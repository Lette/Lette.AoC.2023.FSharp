namespace Lette.AoC2023

open Xunit
open FsUnit.Xunit

module Day06Tests =

    open Day06

    let input = "Time:      7  15   30
Distance:  9  40  200"

    [<Fact>]
    let ``Parser works`` () =
        let result = parse input ()

        result |> should equal [ (7L, 9L); (15L, 40L); (30L, 200L) ]

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 288

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 71503L
