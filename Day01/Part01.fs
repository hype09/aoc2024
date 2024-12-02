module Day01.Part01

let splitInputLine (line: string) = line.Split("   ")
let arrToTuple (arr: 'a array) = arr.[0], arr.[1]
let castTupleToInt (a, b) = int a, int b
let calcDistance (a, b) = abs (a - b)

let solution =
    System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "/input01.txt")
    |> Seq.map (splitInputLine >> arrToTuple >> castTupleToInt)
    |> fun (seq) -> seq |> Seq.map fst, seq |> Seq.map snd
    |> fun (l1, l2) -> (Seq.sort l1, Seq.sort l2)
    ||> Seq.zip
    |> Seq.map calcDistance
    |> Seq.sum
