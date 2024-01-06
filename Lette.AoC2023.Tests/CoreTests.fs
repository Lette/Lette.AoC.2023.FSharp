namespace Lette.AoC2023

open Xunit
open FsUnit.Xunit

module CoreTests =

    [<Fact>]
    let ``Foldi works`` () =

        let array = array2D [[8; 7]; [11; 12]]

        let result = array |> Array2D.foldi (fun s x y v -> s + (x + y) * v) 0

        result |> should equal 42
