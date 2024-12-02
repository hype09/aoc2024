module Day01.Part02

let solution =
    let incr = Option.map ((+) 1) >> Option.defaultValue 1 >> Some
    System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "/input02.txt")
    |> Seq.map (_.Split("   ") >> fun xs -> int xs.[0], int xs.[1])
    |> Seq.fold (fun (seq, m) (a, b) -> (seq @ [ a ], Map.change b incr m)) (List.empty, Map.empty)
    |> fun (xs, m) -> Seq.map (fun a -> (m.TryFind a) |> Option.defaultValue 0 |> (*) a) xs |> Seq.sum
