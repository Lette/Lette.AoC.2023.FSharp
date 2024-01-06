namespace Lette.AoC2023

open System.Diagnostics

module Timing =

    let displayTimerProperties () =

        if Stopwatch.IsHighResolution then
            printfn "Operations timed using the system's high-resolution performance counter."
        else
            printfn "Operations timed using the DateTime class."

        let frequency = Stopwatch.Frequency
        printfn $"  Timer frequency in ticks per second = {frequency}"

        let nanosecPerTick = (1000L*1000L*1000L) / frequency;
        printfn $"  Timer is accurate within {nanosecPerTick} nanoseconds"

        printfn ""
