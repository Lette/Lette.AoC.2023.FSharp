namespace Lette.AoC2023

open System
open System.Diagnostics

type RunnerResult =
    | Parsed   of Day: int                                                * RunTime: TimeSpan
    | Unknown  of Day: int * Part: int * Value: string                    * RunTime: TimeSpan
    | Fail     of Day: int * Part: int * Value: string * Expected: string * RunTime: TimeSpan
    | Success  of Day: int * Part: int * Value: string                    * RunTime: TimeSpan

type IPuzzle =
    abstract member Day: int with get
    abstract member Parse: unit -> RunnerResult
    abstract member RunPart1: unit -> RunnerResult
    abstract member RunPart2: unit -> RunnerResult

type Puzzle<'input, 'output1, 'output2 when 'output1: equality and 'output2: equality> (day, parser, runner1, expected1, runner2, expected2) =

    let parseResult: ref<('input * TimeSpan) option> = ref None

    let timedRunner f x =
        let stopWatch = Stopwatch ()
        stopWatch.Start ()
        let result = f x
        stopWatch.Stop ()
        (result, stopWatch.Elapsed)

    let runParser parser =
        let input, elapsed = timedRunner parser ()
        parseResult.Value <- Some (input, elapsed)
        Parsed (day, elapsed)

    let createResult part actual expected elapsed transform =
        let actual' = transform actual

        match expected with
        | None                                  -> Unknown (day, part, actual',                     elapsed)
        | Some expected when expected <> actual -> Fail    (day, part, actual', transform expected, elapsed)
        | _                                     -> Success (day, part, actual',                     elapsed)

    let runRunner part runner expected =
        match parseResult.Value with
        | None            -> failwith "can't do that"
        | Some (input, _) ->
            let actual, elapsed = timedRunner runner input
            createResult part actual expected elapsed string

    static member init day parser runner1 (expected1: 'output1 option) runner2 (expected2: 'output2 option) =
        Puzzle<'input, 'output1, 'output2> (day, parser day, runner1, expected1, runner2, expected2)

    interface IPuzzle with
        member _.Day with get () = day
        member _.Parse () = runParser parser
        member _.RunPart1 () = runRunner 1 runner1 expected1
        member _.RunPart2 () = runRunner 2 runner2 expected2

[<RequireQualifiedAccess>]
module Puzzle =

    let isUnsuccessful = function
        | Fail _ -> true
        | _      -> false

    let run puzzles settings =

        let latest = puzzles |> List.map (fun (p: IPuzzle) -> p.Day) |> List.max

        let shouldRunDay day =
            match settings.Days with
            | All       -> true
            | Only ds   -> List.contains day ds
            | Except ds -> not (List.contains day ds)
            | Latest    -> day = latest

        let shouldRunPart part =
            match settings.Parts with
            | Both   -> true
            | First  -> part = 1
            | Second -> part = 2

        let runPuzzle (puzzle: IPuzzle) =
            seq {
                if not (shouldRunDay puzzle.Day) then
                    ()
                else
                    let parseResult = puzzle.Parse ()
                    if settings.ShowParserTime then yield parseResult

                    if shouldRunPart 1 then yield puzzle.RunPart1 ()

                    puzzle.Parse () |> ignore // run again for the few cases where first part mutates the input
                    if shouldRunPart 2 then yield puzzle.RunPart2 ()
            }

        seq {
            let puzzles' = puzzles |> List.toSeq

            for p in puzzles' do
                yield! runPuzzle p
        }
