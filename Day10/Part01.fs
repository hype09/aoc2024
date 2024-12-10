module Day10.Part01

type Map = int array array

let trailheads (m: Map) =
    Array.mapi (fun y row ->
        row
        |> Array.mapi (fun x cell -> if cell = 0 then Some (x, y) else None)
        |> Array.choose id
    ) m
    |> Array.concat
    
let findNextCells (m: Map) (l: int) (x: int, y: int) =
    [for x', y' in [(0, 1); (0, -1); (1, 0); (-1, 0)] do yield (x + x', y + y')]
    |> Seq.filter (fun (x, y) -> x >= 0 && x < m[0].Length && y >= 0 && y < m.Length)
    |> Seq.filter (fun (x, y) -> m[y].[x] = l)
    
let rec countTrails (m: Map) (tx: int, ty: int)=
    match m[ty].[tx] with
    | 9 -> Set [(tx, ty)]
    | l -> Seq.fold (fun acc t' -> Set.union acc (countTrails m t')) Set.empty (findNextCells m (l + 1) (tx, ty)) 

let solution =
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input01.txt")
    |> Array.map (Seq.toArray >> Array.map (string >> int))
    |> fun m -> m, trailheads m
    |> fun (m, xs) -> Seq.map (countTrails m) xs
    |> Seq.map _.Count 
    |> Seq.sum
