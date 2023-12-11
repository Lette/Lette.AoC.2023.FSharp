namespace Lette.AoC2023

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day10 =

    type PipeType =
        | Vertical
        | Horizontal
        | NorthEast
        | SouthEast
        | SouthWest
        | NorthWest

    type Cell =
        | Start
        | Empty
        | Pipe of PipeType

    type LoopCrossingDirection =
        | FromNorth
        | FromSouth
        | None

    type RunDirection =
        | North
        | East
        | South
        | West

    let parse input () =

        let startP      = pchar 'S' >>% Start
        let emptyP      = pchar '.' >>% Empty
        let verticalP   = pchar '|' >>% Pipe Vertical
        let horizontalP = pchar '-' >>% Pipe Horizontal
        let northEastP  = pchar 'L' >>% Pipe NorthEast
        let southEastP  = pchar 'F' >>% Pipe SouthEast
        let southWestP  = pchar '7' >>% Pipe SouthWest
        let northWestP  = pchar 'J' >>% Pipe NorthWest

        let rowP = many (startP <|> emptyP <|> verticalP <|> horizontalP <|> northEastP <|> southEastP <|> southWestP <|> northWestP)

        let allP = sepBy' rowP newlineP

        Parser.run allP input
        |> array2D


    let getCell arr (x, y) =
        Array2D.get arr x y

    let setCell arr (x, y) value =
        Array2D.set arr x y value

    let recreateStartCell arr =

        let sizeX = arr |> Array2D.length1
        let sizeY = arr |> Array2D.length2

        let matchingVerticalTop =     [ Pipe Vertical;   Pipe SouthEast; Pipe SouthWest ]
        let matchingVerticalBottom =  [ Pipe Vertical;   Pipe NorthEast; Pipe NorthWest ]
        let matchingHorizontalLeft =  [ Pipe Horizontal; Pipe NorthEast; Pipe SouthEast ]
        let matchingHorizontalRight = [ Pipe Horizontal; Pipe NorthWest; Pipe SouthWest ]

        let x, y = arr |> Array2D.find Start

        let hasTopConnection =
            x > 0         && matchingVerticalTop     |> List.contains (getCell arr ((x - 1),  y     ))
        let hasBottomConnection =
            x < sizeX - 1 && matchingVerticalBottom  |> List.contains (getCell arr ((x + 1),  y     ))
        let hasLeftConnection =
            y > 0         && matchingHorizontalLeft  |> List.contains (getCell arr ( x,      (y - 1)))
        let hasRightConnection =
            y < sizeY - 1 && matchingHorizontalRight |> List.contains (getCell arr ( x,      (y + 1)))

        let pipeAtStart =
            match hasTopConnection, hasBottomConnection, hasLeftConnection, hasRightConnection with
            | true, true, _, _ -> Pipe Vertical
            | _, _, true, true -> Pipe Horizontal
            | true, _, _, true -> Pipe NorthEast
            | true, _, true, _ -> Pipe NorthWest
            | _, true, _, true -> Pipe SouthEast
            | _, true, true, _ -> Pipe SouthWest
            | _ -> failwith "invalid start cell connections"

        setCell arr (x, y) pipeAtStart

        (x, y)

    let findStartDirection arr cell =
        match getCell arr cell with
        | Pipe Vertical   -> North
        | Pipe Horizontal -> East
        | Pipe NorthEast  -> North
        | Pipe SouthEast  -> East
        | Pipe SouthWest  -> South
        | Pipe NorthWest  -> North
        | c -> failwith (sprintf "unexpected non-pipe at start position: %A" c)

    let getPipeAt arr cell =
        match getCell arr cell with
        | Pipe p -> p
        | c      -> failwith (sprintf "unexpected non-pipe at loop position: %A" c)

    let findLoop arr startPosition =

        let findNextLoopCell (x, y) direction =
            match direction with
            | North -> (x - 1, y    )
            | East  -> (x    , y + 1)
            | South -> (x + 1, y    )
            | West  -> (x    , y - 1)

        let findNextDirection cell fromDirection =
            match getCell arr cell, fromDirection with
            | Pipe Vertical  , North -> North
            | Pipe Vertical  , South -> South
            | Pipe Horizontal, East  -> East
            | Pipe Horizontal, West  -> West
            | Pipe NorthEast , South -> East
            | Pipe NorthEast , West  -> North
            | Pipe SouthEast , West  -> South
            | Pipe SouthEast , North -> East
            | Pipe SouthWest , East  -> South
            | Pipe SouthWest , North -> West
            | Pipe NorthWest , East  -> North
            | Pipe NorthWest , South -> West
            | p              , d     -> failwith (sprintf "unexpected pipe and direction combination - pipe: %A, from: %A" p d)

        let rec loop current direction doExitCheck acc =
            if doExitCheck && current = startPosition && not (List.isEmpty acc) then
                acc
            else
                let nextCell = findNextLoopCell current direction
                let nextDirection = findNextDirection nextCell direction

                loop nextCell nextDirection true ((current, getPipeAt arr current) :: acc)

        loop startPosition (findStartDirection arr startPosition) false []

    let removeNonLoopPipes arr loop =

        Array2D.iteri (fun x y _ -> setCell arr (x, y) Empty) arr
        loop |> List.iter (fun (cell, pipe) -> setCell arr cell (Pipe pipe))

    let countInsideCells arr =

        let countInsidesInRow _ row =

            let rowFolder (count, isOutside, loopDirection) cell =
                match cell, isOutside, loopDirection with
                | Empty          , false, _         -> (count + 1, false        , None)
                | Empty          , true , _         -> (count    , true         , None)
                | Pipe Vertical  , _    , None      -> (count    , not isOutside, None)
                | Pipe Horizontal, _    , _         -> (count    ,     isOutside, loopDirection)
                | Pipe NorthEast , _    , None      -> (count    ,     isOutside, FromNorth)
                | Pipe NorthWest , _    , FromNorth -> (count    ,     isOutside, None)
                | Pipe SouthEast , _    , None      -> (count    ,     isOutside, FromSouth)
                | Pipe SouthWest , _    , FromSouth -> (count    ,     isOutside, None)
                | Pipe SouthWest , _    , FromNorth -> (count    , not isOutside, None)
                | Pipe NorthWest , _    , FromSouth -> (count    , not isOutside, None)
                | Pipe p         , _    , d         -> failwith (sprintf "unexpected pipe combination - pipe: %A, from: %A" p d)
                | _              , _    , _         -> (count    ,     isOutside, None)

            Array.fold rowFolder (0, true, None) row
            |> fun (count, _, _) -> count

        Array2D.mapiRows countInsidesInRow arr
        |> List.sum

    let part1 input =

        let startPosition = recreateStartCell input
        let loop = findLoop input startPosition

        List.length loop / 2

    let part2 input =

        let startPosition = recreateStartCell input
        let loop = findLoop input startPosition

        removeNonLoopPipes input loop
        countInsideCells input

    let puzzle =
        Puzzle.init
            10
            (getInput >> parse)
            part1 (Some 6757)
            part2 (Some 523)
