namespace Lette.AoC2023

open Xunit
open FsUnit.Xunit

module Day02Tests =

    open Day02

    let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

    [<Fact>]
    let ``Parser works`` () =

        let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"

        let result = parse input ()

        result |> should equal [
            Game (1, [
                Hand [ (Blue,  3); (Red,   4) ]
                Hand [ (Red,   1); (Green, 2); (Blue, 6) ]
                Hand [ (Green, 2) ]
            ])
            Game (2, [
                Hand [ (Blue,  1); (Green, 2) ]
                Hand [ (Green, 3); (Blue,  4); (Red,  1) ]
                Hand [ (Green, 1); (Blue,  1) ]
            ])
        ]

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 8

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 2286
