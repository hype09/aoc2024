module Day08.Part01

let findAntiNodes (m: char array array) f=
    Array.mapi (fun y row ->
        row
        |> Array.mapi (fun x cell -> if cell = f then Some (x, y) else None)
        |> Array.choose id
    ) m
    |> Array.concat
    |> fun fps -> (
        Array.collect (fun a ->
        fps 
        |> Array.filter (fun b -> a <> b) 
        |> Array.map (fun b -> a, b)
        ) fps
    )
    |> Array.map (fun ((x, y), (x',y')) -> x + (x-x'), y + (y-y'))
    |> Array.filter (fun (x, y) -> x >= 0 && x < m[0].Length && y >= 0 && y < m.Length)
    |> Set

let solution =
    let m = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input01.txt") |> Seq.map Seq.toArray |> Seq.toArray
    Set.fold (fun acc f -> Set.union acc (findAntiNodes m f)) Set.empty (m |> Array.concat |> Set |> _.Remove('.'))
    |> Set.count