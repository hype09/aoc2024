module Day02.Part01

let lineToIntArray (line: string) = line.Split(" ") |> Array.map int

let isAsc arr = arr |> Seq.windowed 2 |> Seq.forall (fun [|a;b|] -> a < b && (b - a) <= 3)
let isDesc arr = arr |> Seq.windowed 2 |> Seq.forall (fun [|a;b|] -> a > b && (a - b) <= 3)
    
let solution =
    System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "/input01.txt")
    |> Seq.map lineToIntArray
    |> Seq.filter (fun arr -> isAsc arr || isDesc arr)
    |> Seq.length
