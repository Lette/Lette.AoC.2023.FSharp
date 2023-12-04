namespace Lette.AoC2023

[<AutoOpen>]
module Core =

    let flip f a b = f b a

    let cons x xs = x :: xs
    let cons' (x, xs) = cons x xs

    let invoke f = f ()

    let dump x = printfn $"%A{x}"

    let countDistinctItems xss =

        let updateCounts items map =

            let updater =
                Option.map ((+) 1) >> Option.orElse (Some 1)

            let updateCount item =
                Map.change item updater

            let rec update items acc =
                match items with
                | []      -> acc
                | x :: xs -> update xs (updateCount x acc)

            update items map

        let folder map items =
            updateCounts (List.distinct items) map

        xss
        |> List.fold folder Map.empty
        |> Map.toList

    let findCommonItem xss =

        xss
        |> countDistinctItems
        |> List.where (snd >> (=) (List.length xss))
        |> List.head
        |> fst

    let (|IsInteger|_|) (s: string) =
        match System.Int32.TryParse s with
        | false, _      -> None
        | true , result -> Some result

[<RequireQualifiedAccess>]
module List =

    let tap f xs = List.map (fun x -> f x; x) xs

[<RequireQualifiedAccess>]
module Seq =

    let tap f xs = Seq.map (fun x -> f x; x) xs

[<RequireQualifiedAccess>]
module Tuple =

    let map fa fb (a, b) = (fa a, fb b)

[<RequireQualifiedAccess>]
module Array2D =

    let foldi<'t, 'state> (folder: 'state -> int -> int -> 't -> 'state) (state: 'state) (array: 't[,]) =

        let sizeX = Array2D.length1 array
        let sizeY = Array2D.length2 array

        let nextIndex (x, y) =
            let y' = y + 1
            if y' = sizeY then (x + 1, 0) else (x, y')

        let rec loop (x, y) acc =
            let v = array[x, y]
            let acc' = folder acc x y v
            let x', y' = nextIndex (x, y)
            if x' = sizeX then
                acc'
            else
                loop (x', y') acc'

        loop (0, 0) state

    let find item arr =
        let maxX = arr |> Array2D.length1
        let maxY = arr |> Array2D.length2

        let rec find' x y =
            match x, y with
            | _, y when y = maxY -> find' (x + 1) 0
            | x, _ when x = maxX -> failwith "could not find item"
            | _                  -> if arr[x, y] = item then (x, y) else find' x (y + 1)

        find' 0 0

    let tryFind item arr =
        let maxX = arr |> Array2D.length1
        let maxY = arr |> Array2D.length2

        let rec find' x y =
            match x, y with
            | _, y when y = maxY -> find' (x + 1) 0
            | x, _ when x = maxX -> None // failwith "could not find item"
            | _                  -> if arr[x, y] = item then Some (x, y) else find' x (y + 1)

        find' 0 0

    let count item arr =
        let maxX = arr |> Array2D.length1
        let maxY = arr |> Array2D.length2

        let rec find' x y acc =
            match x, y with
            | _, y when y = maxY -> find' (x + 1) 0 acc
            | x, _ when x = maxX -> acc
            | _                  -> find' x (y + 1) (acc + if arr[x, y] = item then 1 else 0)

        find' 0 0 0

    let mapiRows f arr =
        let height = arr |> Array2D.length1

        [
            for x in 0 .. height - 1 do
                yield f x arr[x, *]
        ]

    let mapRows f arr =
        let height = arr |> Array2D.length1

        [
            for x in 0 .. height - 1 do
                yield f arr[x, *]
        ]

    let toListOfLists arr =
        arr
        |> mapRows Array.toList

[<RequireQualifiedAccess>]
module String =
    let indexOfAny chars (str : string) = str.IndexOfAny chars
    let lastIndexOfAny chars (str : string) = str.LastIndexOfAny chars
    let getChar index (str : string) = str[index]
    let replace (a : string) (b : string) (str : string) = str.Replace (a, b)
