namespace Lette.AoC2023

open Xunit
open FsUnit.Xunit

module Day11Tests =

    open Day11

    let input = "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."

    [<Fact>]
    let ``Parser works`` () =
        let input = "...#.
#...."

        let result = parse input ()

        result |> should equal [
           [ Empty; Empty; Empty; Galaxy; Empty ]
           [ Galaxy; Empty; Empty; Empty; Empty ]
        ]

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 374L

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 100L (parse input ())

        result |> should equal 8410L
