namespace Lette.AoC2023

open Xunit
open FsUnit.Xunit

module Day07Tests =

    open Day07

    let input = "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"

    [<Fact>]
    let ``Parser works`` () =
        let input = "32T3K 765
T55J5 684"

        let result = parse input ()

        result |> should equal [
            ([ 3; 2; 10; 3; 13 ], 765)
            ([ 10; 5; 5; 11; 5 ], 684)
        ]

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 6440

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 5905
