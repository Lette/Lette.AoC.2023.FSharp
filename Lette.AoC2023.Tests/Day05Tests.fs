namespace Lette.AoC2023

open Xunit
open FsUnit.Xunit

module Day05Tests =

    open Day05

    let input = "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"

    [<Fact>]
    let ``Parser works`` () =
        let input = "seeds: 79 14

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37"

        let result = parse input ()

        result |> should equal
            {
                Seeds = [ 79; 14 ]
                Mappers = [
                    {
                        SourceCategory = "seed"
                        DestinationCategory = "soil"
                        Mappings = [
                            { SourceRangeStart = 98; DestinationRangeStart  = 50; RangeLength = 2 }
                            { SourceRangeStart = 50; DestinationRangeStart  = 52; RangeLength = 48 }
                        ]
                    }
                    {
                        SourceCategory = "soil"
                        DestinationCategory = "fertilizer"
                        Mappings = [
                            { SourceRangeStart = 15; DestinationRangeStart  = 0; RangeLength = 37 }
                        ]
                    }
                ]
            }

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 35L

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 46L
