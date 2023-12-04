namespace Lette.AoC2023

open Xunit
open FsUnit.Xunit

module Day04Tests =

    open Day04

    let input = "Card  1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card  2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card  3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card  4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card  5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card  6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

    [<Fact>]
    let ``Parser works`` () =
        let input = "Card  1: 41  8 83 | 83 86  6 31 17
Card  2: 13 32 20 | 61 30 68 82 17"

        let result = parse input ()

        result |> should equal [
            Card (CardNumber 1, Winning [ 41;  8; 83 ], Played [ 83; 86;  6; 31; 17 ])
            Card (CardNumber 2, Winning [ 13; 32; 20 ], Played [ 61; 30; 68; 82; 17 ])
        ]

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 13

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 30
