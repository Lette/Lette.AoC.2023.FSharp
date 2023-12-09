namespace Lette.AoC2023

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day08 =

    let parse input () =

        let leftP = pchar 'L' >>% 0
        let rightP = pchar 'R' >>% 1
        let directionP = leftP <|> rightP
        let directionsP = many directionP

        let nodeP = anyString 3

        let instructionP = nodeP .>> pstring " = (" .>>. nodeP .>> pstring ", " .>>. nodeP .>> pstring ")" |>> fun ((n, nl), nr) -> (n, [| nl; nr |])
        let instructionsP = sepBy' instructionP newlineP

        let allP = directionsP .>> newline .>> newline .>>. instructionsP

        Parser.run allP input
        |> fun (directions, instructions) -> (directions, Map.ofList instructions)

    let inline endsWith c = String.getChar 2 >> (=) c

    let runDirections directions instructions exitCondition node =

        let nextNode node direction =
            instructions
            |> Map.find node
            |> (flip Array.get direction)

        let rec loop remainingDirections currentNode steps =
            if exitCondition currentNode then
                steps
            else
                match remainingDirections with
                | []      -> loop directions currentNode              steps
                | d :: ds -> loop ds         (nextNode currentNode d) (steps + 1L)

        loop directions node 0L

    let part1 (directions, instructions) =

        runDirections directions instructions ((=) "ZZZ") "AAA"

    let part2 (directions, instructions) =

        instructions
        |> Map.keys
        |> Seq.filter (endsWith 'A')
        |> List.ofSeq
        |> List.map (runDirections directions instructions (endsWith 'Z'))
        |> List.reduce lcm

    let puzzle =
        Puzzle.init
            8
            (getInput >> parse)
            part1 (Some 19099L)
            part2 (Some 17099847107071L)
