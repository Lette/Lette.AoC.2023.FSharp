namespace Lette.AoC2023

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day05 =

    type Mapping =
        {
            SourceRangeStart: int64
            DestinationRangeStart: int64
            RangeLength: int64
        }

    let toMapping ((dest, source), length) =
        {
            SourceRangeStart = source
            DestinationRangeStart = dest
            RangeLength = length
        }

    type Mapper =
        {
            SourceCategory: string
            DestinationCategory: string
            Mappings: Mapping list
        }

    let toMapper ((sourceCategory, destinationCategory), mappings) =
        {
            SourceCategory = sourceCategory
            DestinationCategory = destinationCategory
            Mappings = mappings
        }

    type Almanac =
        {
            Seeds : int64 list
            Mappers: Mapper list
        }

    let toAlmanac (seeds, mappers) =
        {
            Seeds = seeds
            Mappers = mappers
        }

    let parse input () =

        let seedsP = pstring "seeds: " >>. sepBy pint64 spaceP

        let mapperCategoriesP = many1Chars (noneOf "-") .>> pstring "-to-" .>>. many1Chars (noneOf " ") .>> pstring " map:"
        let mappingP = pint64 .>> spaceP .>>. pint64 .>> spaceP .>>. pint64 |>> toMapping
        let mappingsP = sepBy' mappingP newlineP
        let mapperP = mapperCategoriesP .>> newlineP .>>. mappingsP |>> toMapper

        let mappersP = sepBy' mapperP (newlineP .>> newlineP)

        let almanacP = seedsP .>> newlineP .>> newlineP .>>. mappersP |>> toAlmanac

        Parser.run almanacP input

    let toMapperFn { Mappings = mappings } =

        (fun input ->

            let tryFindMatch { SourceRangeStart = sourceStart; DestinationRangeStart = destStart; RangeLength = rangeLength } input =
                if sourceStart <= input && input < sourceStart + rangeLength then
                    Some (destStart + input - sourceStart)
                else
                    None

            let rec findOutput mappings =
                match mappings with
                | []      -> input
                | m :: ms ->
                    match tryFindMatch m input with
                    | Some output -> output
                    | None        -> findOutput ms

            findOutput mappings
        )

    let mappers input =
        input.Mappers
        |> List.map toMapperFn

    let part1 input =

        input.Seeds
        |> List.map (fun seed -> (mappers input) |> List.fold (fun input mapper -> mapper input) seed)
        |> List.min

    let part2 input =

        let getSeedsSeq seeds =

            let rec loop seeds =
                seq {
                    match seeds with
                    | []      -> ()
                    | _ :: [] -> ()
                    | s1 :: s2 :: ss ->
                        yield! { s1 .. (s1 + s2 - 1L) }
                        yield! loop ss
                }

            loop seeds

        let seeds = seq { yield! getSeedsSeq input.Seeds }

        seeds
        |> Seq.map (fun seed -> (mappers input) |> List.fold (fun input mapper -> mapper input) seed)
        |> Seq.min

    let puzzle =
        Puzzle.init
            5
            (getInput >> parse)
            part1 (Some 309796150L)
            part2 (Some 50716416L)
