module Day01.Part02

let solution =
    let incr = Option.map ((+) 1) >> Option.defaultValue 1 >> Some
    System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "/input02.txt")
    |> Seq.fold (fun (seq, m) s -> s.Split("   ") |> fun xs -> (int xs.[0] :: seq, Map.change (int xs.[1]) incr m)) ([], Map.empty)
    |> fun (xs, m) -> Seq.map (fun a -> (m.TryFind a) |> Option.defaultValue 0 |> (*) a) xs |> Seq.sum
