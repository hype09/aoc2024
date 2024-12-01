let day01_1 =
    System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "/day01/input.txt")
    |> Seq.map (fun line -> line.Split("   "))
    |> Seq.map (fun arr -> int arr.[0], int arr.[1])
    |> Seq.toList
    |> List.unzip
    |> fun (l1, l2) -> (List.sort l1, List.sort l2)
    |> fun (l1, l2) -> List.zip l1 l2
    |> List.map (fun (a, b) -> abs (a - b))
    |> List.sum

printfn "Solution for Day 1: %A" day01_1