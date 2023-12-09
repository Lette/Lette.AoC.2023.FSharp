namespace Lette.AoC2023

module Program =

    open Settings

    let parseArgs args =
        let args' = args |> List.ofArray

        let rec loop args acc =
            match args with
            | [] -> acc

            | "-l"                        :: xs -> loop xs { acc with Days = Latest }
            | "--latest"                  :: xs -> loop xs { acc with Days = Latest }

            | "-a"                        :: xs -> loop xs { acc with Days = All; Parts = Both }
            | "--all"                     :: xs -> loop xs { acc with Days = All; Parts = Both }

            | "-d"                        :: [] -> failwith "-d must be followed by a valid day"
            | "-d"       :: IsInteger day :: xs -> loop xs { acc with Days = setOnly day acc.Days }
            | "-d"       :: bad           :: _  -> failwith (sprintf "'%s' is an unknown day" bad)
            | "--day"                     :: [] -> failwith "--day must be followed by a valid day"
            | "--day"    :: IsInteger day :: xs -> loop xs { acc with Days = setOnly day acc.Days }
            | "--day"    :: bad           :: _  -> failwith (sprintf "'%s' is an unknown day" bad)

            | "-e"                        :: [] -> failwith "-e must be followed by a valid day"
            | "-e"       :: IsInteger day :: xs -> loop xs { acc with Days = setExcept day acc.Days}
            | "-e"       :: bad           :: _  -> failwith (sprintf "'%s' is an unknown day" bad)
            | "--except"                  :: [] -> failwith "--except must be followed by a valid day"
            | "--except" :: IsInteger day :: xs -> loop xs { acc with Days = setExcept day acc.Days}
            | "--except" :: bad           :: _  -> failwith (sprintf "'%s' is an unknown day" bad)

            | "-p"                        :: [] -> failwith "-p must be followed by a valid part (1 or 2)"
            | "-p"       :: "1"           :: xs -> loop xs { acc with Parts = First }
            | "-p"       :: "2"           :: xs -> loop xs { acc with Parts = Second }
            | "-p"       :: bad           :: _  -> failwith (sprintf "'%s' is an unknown part" bad)
            | "--part"                    :: [] -> failwith "--part must be followed by a valid part (1 or 2)"
            | "--part"   :: "1"           :: xs -> loop xs { acc with Parts = First }
            | "--part"   :: "2"           :: xs -> loop xs { acc with Parts = Second }
            | "--part"   :: bad           :: _  -> failwith (sprintf "'%s' is an unknown part" bad)

            | "-t"                        :: xs -> loop xs { acc with ShowParserTime = true }
            | "--show-parser-time"        :: xs -> loop xs { acc with ShowParserTime = true }

            | s                           :: _  -> failwith (sprintf "'%s' is an unknown argument" s)

        loop args' defaultSettings

    [<EntryPoint>]
    let main args =

        let settings = parseArgs args

        Timing.displayTimerProperties ()

        let puzzles =
            [
                Day01.puzzle
                Day02.puzzle
                Day03.puzzle
                Day04.puzzle
                Day05.puzzle
                Day06.puzzle
                Day07.puzzle
                Day08.puzzle
                Day09.puzzle
            ] : IPuzzle list

        Presentation.printHeader ()

        Puzzle.run puzzles settings
        |> Seq.tap Presentation.printResult
        |> Seq.where Puzzle.isUnsuccessful
        |> Seq.length
