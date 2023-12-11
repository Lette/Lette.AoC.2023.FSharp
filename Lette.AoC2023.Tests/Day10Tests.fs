namespace Lette.AoC2023

open Xunit
open FsUnit.Xunit

module Day10Tests =

    open Day10

    let input = "..F7.
.FJ|.
SJ.L7
|F--J
LJ..."

    [<Fact>]
    let ``Parser works`` () =
        let result = parse input ()

        result |> should equal (array2D [
            [ Empty; Empty; Pipe SouthEast; Pipe SouthWest; Empty ]
            [ Empty; Pipe SouthEast; Pipe NorthWest; Pipe Vertical; Empty ]
            [ Start; Pipe NorthWest; Empty; Pipe NorthEast; Pipe SouthWest ]
            [ Pipe Vertical; Pipe SouthEast; Pipe Horizontal; Pipe Horizontal; Pipe NorthWest ]
            [ Pipe NorthEast; Pipe NorthWest; Empty; Empty; Empty ]
        ])

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 8

    [<Fact>]
    let ``Part 2`` () =

        let input = "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L"

        let result = part2 (parse input ())

        result |> should equal 10
