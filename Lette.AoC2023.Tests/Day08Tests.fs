namespace Lette.AoC2023

open Xunit
open FsUnit.Xunit

module Day08Tests =

    open Day08

    let input = "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"

    [<Fact>]
    let ``Parser works`` () =
        let result = parse input ()

        result |> should equal (
            [ 0; 0; 1 ],
            [
                ("AAA", [|"BBB"; "BBB"|])
                ("BBB", [|"AAA"; "ZZZ"|])
                ("ZZZ", [|"ZZZ"; "ZZZ"|])
            ] |> Map.ofList
        )

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 6L

    [<Fact>]
    let ``Part 2`` () =

        let input = "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"

        let result = part2 (parse input ())

        result |> should equal 6L
