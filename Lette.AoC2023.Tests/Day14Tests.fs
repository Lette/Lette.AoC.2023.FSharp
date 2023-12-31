namespace Lette.AoC2023

open Xunit
open FsUnit.Xunit

module Day14Tests =

    open Day14

    let input = "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."

    [<Fact>]
    let ``Parser works`` () =
        let input = "O..#.
O.OO#"

        let result = parse input ()

        result |> should equal (array2D [
            [ RoundedRock; Empty; Empty; CubeShapedRock; Empty ]
            [ RoundedRock; Empty; RoundedRock; RoundedRock; CubeShapedRock ]
        ])

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 136

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 3 (parse input ())

        result |> should equal 69
