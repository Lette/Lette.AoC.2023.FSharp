namespace Lette.AoC2023

open System

[<RequireQualifiedAccess>]
module Presentation =

    let private getFormattedResult pr =

        let parsedTemplate  day                      elapsed = $"       | %3i{day} |      |  %8.1f{elapsed} | parsed input"
        let unknownTemplate day part actual          elapsed = $" ????  | %3i{day} | %4i{part} |  %8.1f{elapsed} | %s{actual}"
        let failTemplate    day part actual expected elapsed = $" FAIL  | %3i{day} | %4i{part} |  %8.1f{elapsed} | %s{actual} != %s{expected}"
        let successTemplate day part actual          elapsed = $" Pass  | %3i{day} | %4i{part} |  %8.1f{elapsed} | %s{actual}"

        let formattedParser day elapsed =
            (parsedTemplate day elapsed, Some ConsoleColor.DarkGreen)

        let formattedUnknown day part actual elapsed =
            (unknownTemplate day part actual elapsed, Some ConsoleColor.Blue)

        let formattedFail day part actual expected elapsed =
            (failTemplate day part actual expected elapsed, Some ConsoleColor.Red)

        let formattedSuccess day part actual elapsed =
            (successTemplate day part actual elapsed, Some ConsoleColor.Green)

        match pr with
        | Parsed  (day,                         elapsed) -> formattedParser  day                      elapsed.TotalMilliseconds
        | Unknown (day, part, actual,           elapsed) -> formattedUnknown day part actual          elapsed.TotalMilliseconds
        | Fail    (day, part, actual, expected, elapsed) -> formattedFail    day part actual expected elapsed.TotalMilliseconds
        | Success (day, part, actual,           elapsed) -> formattedSuccess day part actual          elapsed.TotalMilliseconds

    let private cprintf color format =
        let continuation (result : string) =
            let previousColor = Console.ForegroundColor
            try
                Console.ForegroundColor <- color
                Console.Write result
            finally
                Console.ForegroundColor <- previousColor

        Printf.kprintf continuation format

    let printHeader () =
        printfn "Result | Day | Part | Time (ms) | Answer"
        printfn "-------|-----|------|-----------|----------------------------------------------"

    let printResult result =

        let msg, color = result |> getFormattedResult

        match color with
        | None   -> printf    $"%s{msg}"
        | Some c -> cprintf c $"%s{msg}"

        printfn ""
