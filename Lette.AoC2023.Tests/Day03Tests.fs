namespace Lette.AoC2023

open Xunit
open FsUnit.Xunit

module Day03Tests =

    open Day03

    let input = "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."

    [<Fact>]
    let ``Parser works`` () =
        let input = ".23.*
4.#.1"

        let result = parse input ()

        result |> Array2D.toListOfLists |> should equal [
            [ Empty; Digit 2; Digit 3; Empty; Symbol '*' ]
            [ Digit 4; Empty; Symbol '#'; Empty; Digit 1 ]
        ]

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 4361

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 467835
