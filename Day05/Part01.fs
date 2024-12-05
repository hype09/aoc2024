module Day05.Part01

open System.Text.RegularExpressions

let parseRule (l: string) = l.Split('|') |> Array.map int |> fun xs -> xs[0], xs[1]
let parseUpdate (l: string) = l.Split(',') |> Array.map int |> Array.toList

let parseInput = 
    Seq.fold (fun (rs, us) (l: string) ->
        if Regex.IsMatch(l, "\d+\|\d+") then (parseRule l :: rs, us)
        elif Regex.IsMatch(l, "\d+(?:,\d+)*") then (rs, parseUpdate l :: us)
        else (rs, us)
    ) (List.empty, List.empty)

let sortUpdate rs = List.sortWith (fun a b -> if (List.contains (a, b) rs) then -1 else 1)
let isUpdateValid rs (u: int list) = u = sortUpdate rs u

let solution =
    System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "/input01.txt")
    |> parseInput
    |> fun (rs, us) -> List.filter (isUpdateValid rs) us |> List.map (fun u -> u[u.Length / 2]) |> List.sum 