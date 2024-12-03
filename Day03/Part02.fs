module Day03.Part02

open System.Text.RegularExpressions

type MulState = Enabled | Disabled

let solution =
    System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/input01.txt")
    |> fun s -> Regex.Matches(s, "mul\((\d\d?\d?),(\d\d?\d?)\)|(do\(\))|(don't\(\))")
    |> Seq.fold (fun (acc, mulState) m -> match m.Value with
                                            | "do()" -> (acc, Enabled)
                                            | "don't()" -> (acc, Disabled)
                                            | _ -> if mulState = Disabled then (acc, mulState)
                                                   else (acc + int m.Groups.[1].Value * int m.Groups.[2].Value, mulState)
    ) (0, Enabled)
    |> fst