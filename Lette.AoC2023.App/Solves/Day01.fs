namespace Lette.AoC2023

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day01 =

    let parse input () =
        
        let charP = anyOf (['a'..'z'] @ ['0'..'9'])

        let allP = sepBy' (many charP) newlineP

        Parser.run allP input

    let part1 input =

        let findFirst xs =
            let rec find xs =
                match xs with
                | [] -> failwith "no digits"
                | '1' :: _ -> 1
                | '2' :: _ -> 2
                | '3' :: _ -> 3
                | '4' :: _ -> 4
                | '5' :: _ -> 5
                | '6' :: _ -> 6
                | '7' :: _ -> 7
                | '8' :: _ -> 8
                | '9' :: _ -> 9
                | _ :: xs -> find xs

            find xs

        let findLast xs =
            let rec find xs =
                match xs with
                | [] -> failwith "no digits"
                | '1' :: _ -> 1
                | '2' :: _ -> 2
                | '3' :: _ -> 3
                | '4' :: _ -> 4
                | '5' :: _ -> 5
                | '6' :: _ -> 6
                | '7' :: _ -> 7
                | '8' :: _ -> 8
                | '9' :: _ -> 9
                | _ :: xs -> find xs

            find (List.rev xs)

        input
        |> List.map (fun is -> (findFirst is, findLast is))
        |> List.map (fun (a, b) -> a * 10 + b)
        |> List.sum

    let part2 input =

        let findFirst xs =
            let rec find xs =
                match xs with
                | [] -> failwith "no digits"
                | '1' :: _ -> 1
                | '2' :: _ -> 2
                | '3' :: _ -> 3
                | '4' :: _ -> 4
                | '5' :: _ -> 5
                | '6' :: _ -> 6
                | '7' :: _ -> 7
                | '8' :: _ -> 8
                | '9' :: _ -> 9
                | 'o' :: 'n' :: 'e' :: _ -> 1
                | 't' :: 'w' :: 'o' :: _ -> 2
                | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ -> 3
                | 'f' :: 'o' :: 'u' :: 'r' :: _ -> 4
                | 'f' :: 'i' :: 'v' :: 'e' :: _ -> 5
                | 's' :: 'i' :: 'x' :: _ -> 6
                | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ -> 7
                | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ -> 8
                | 'n' :: 'i' :: 'n' :: 'e' :: _ -> 9
                | _ :: xs -> find xs

            find xs

        let findLast xs =
            let rec find xs =
                match xs with
                | [] -> failwith "no digits"
                | '1' :: _ -> 1
                | '2' :: _ -> 2
                | '3' :: _ -> 3
                | '4' :: _ -> 4
                | '5' :: _ -> 5
                | '6' :: _ -> 6
                | '7' :: _ -> 7
                | '8' :: _ -> 8
                | '9' :: _ -> 9
                | 'e' :: 'n' :: 'o' :: _ -> 1
                | 'o' :: 'w' :: 't' :: _ -> 2
                | 'e' :: 'e' :: 'r' :: 'h' :: 't' :: _ -> 3
                | 'r' :: 'u' :: 'o' :: 'f' :: _ -> 4
                | 'e' :: 'v' :: 'i' :: 'f' :: _ -> 5
                | 'x' :: 'i' :: 's' :: _ -> 6
                | 'n' :: 'e' :: 'v' :: 'e' :: 's' :: _ -> 7
                | 't' :: 'h' :: 'g' :: 'i' :: 'e' :: _ -> 8
                | 'e' :: 'n' :: 'i' :: 'n' :: _ -> 9
                | _ :: xs -> find xs

            find (List.rev xs)

        input
        |> List.map (fun is -> (findFirst is, findLast is))
        |> List.map (fun (a, b) -> a * 10 + b)
        |> List.sum

    let puzzle =
        Puzzle.init
            1
            (getInput >> parse)
            part1 (Some 54239)
            part2 (Some 55343)
