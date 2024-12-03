module Day03.Part01

open System.Text.RegularExpressions

let solution =
    System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/input01.txt")
    |> fun s -> Regex.Matches(s, "mul\((\d\d?\d?),(\d\d?\d?)\)")
    |> Seq.fold (fun acc m -> acc + int m.Groups.[1].Value * int m.Groups.[2].Value) 0