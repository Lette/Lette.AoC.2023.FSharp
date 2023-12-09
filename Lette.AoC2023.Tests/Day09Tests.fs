namespace Lette.AoC2023

open Xunit
open FsUnit.Xunit

module Day09Tests =

    open Day09

    let input = "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"

    [<Fact>]
    let ``Parser works`` () =
        let result = parse input ()

        result |> should equal [
            [ 0; 3; 6; 9; 12; 15 ]
            [ 1; 3; 6; 10; 15; 21 ]
            [ 10; 13; 16; 21; 30; 45 ]
        ]

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 114

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 2
