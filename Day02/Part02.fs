module Day02.Part02

let lineToIntArray (line: string) = line.Split(" ") |> Array.map int

let forAllPairs f xs =
    xs |> Seq.windowed 2 |> Seq.forall (fun [| a; b |] -> f a b)

let isAsc = forAllPairs (fun a b -> a < b && (b - a) <= 3)
let isDesc = forAllPairs (fun a b -> a > b && (a - b) <= 3)

let partialCombinations (xs: int array) =
    xs |> Array.mapi (fun i _ -> Array.concat [ xs.[.. i - 1]; xs.[i + 1 ..] ])

let solution =
    System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "/input01.txt")
    |> Seq.map lineToIntArray
    |> Seq.filter (fun arr -> partialCombinations arr |> Seq.exists (fun arr' -> isAsc arr' || isDesc arr'))
    |> Seq.length
