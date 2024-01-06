namespace Lette.AoC2023

open System.IO

[<AutoOpen>]
module Data =

    let getInput day =
        let path = $"{__SOURCE_DIRECTORY__}\Input\Day%02i{day}.txt"

        if not (File.Exists path) then
            failwithf $"input file for day %i{day} not found"
        else
            File.ReadAllText path
