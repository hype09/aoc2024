module Day08.Part02

let isInsideGrid x y (m: char array array) = x >= 0 && x < m[0].Length && y >= 0 && y < m.Length

let resonantHarmonics (m: char array array) (((x,y), (x',y')): (int * int) * (int * int)) =
    let mutable a = x
    let mutable b = y
    let mutable s = Set.empty.Add((x,y)).Add((x',y'))
    while (isInsideGrid a b m) do
        a <- a + x - x'
        b <- b + y - y'
        if isInsideGrid a b m then s <- s.Add((a, b))
    s

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
    |> Array.map (resonantHarmonics m)
    |> Array.fold Set.union Set.empty
    |> Set

let solution =
    let m = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input01.txt") |> Seq.map Seq.toArray |> Seq.toArray
    Set.fold (fun acc f -> Set.union acc (findAntiNodes m f)) Set.empty (m |> Array.concat |> Set |> _.Remove('.'))
    |> Set.count