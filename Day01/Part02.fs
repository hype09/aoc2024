module Day01.Part02

let splitInputLine (line: string) = line.Split("   ")
let arrToTuple (arr: 'a array) = arr.[0], arr.[1]
let castTupleToInt (a, b) = int a, int b

let countOccurrences seq =
    let incr i = Some (i |> Option.defaultValue 0 |> (+) 1)
    Seq.fold (fun acc a -> Map.change a incr acc) Map.empty seq

let calcDistance a b = b |> Option.defaultValue 0 |> (*) a
    
let solution =
    System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "/input02.txt")
    |> Seq.map (splitInputLine >> arrToTuple >> castTupleToInt)
    |> fun seq ->
        let m = seq |> Seq.map snd |> countOccurrences
        seq |> Seq.map (fst >> (fun a -> calcDistance a (m.TryFind a))) |> Seq.sum
