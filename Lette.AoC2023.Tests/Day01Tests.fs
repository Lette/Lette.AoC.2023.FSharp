namespace Lette.AoC2023

open Xunit
open FsUnit.Xunit

module Day01Tests =

    open Day01

    let input = " "

    [<Fact>]
    let ``Parser works`` () =
        let result = parse input ()

        result |> should equal [Some ' ']

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 0

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 0
