module Day11.Part02

let blink (stones: uint64 list) =
    List.fold (fun acc s ->
        match string s with
        | "0" -> 1UL :: acc
        | x when x.Length % 2 = 0 -> (uint64 (x.Substring(0, x.Length / 2))) :: (uint64 (x.Substring(x.Length / 2))) :: acc 
        | x -> (uint64 x) * 2024UL :: acc
    ) [] stones
    |> List.rev

let solution =
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input01.txt")[0]
    |> _.Split(' ')
    |> Array.toList
    |> List.map uint64
    |> fun stones ->
        List.fold (fun s i ->
            printfn "Iteration: %d" i
            blink s
        ) stones [ 1..75 ]
    |> _.Length
