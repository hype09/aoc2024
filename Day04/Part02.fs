module Day04.Part02

open System.Text.RegularExpressions

let solution =
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input01.txt")
    |> Array.map Seq.toArray
    |> Array.windowed 3
    |> Array.map (fun xs -> Array.zip3 xs[0] xs[1] xs[2])
    |> Array.map (fun xs ->
        xs
        |> Array.windowed 3
        |> Array.map ((Array.collect (fun (a, b, c) -> [| a; b; c |])) >> System.String)
        |> Array.filter (fun s -> Regex.IsMatch(s, "M.M.A.S.S|M.S.A.M.S|S.M.A.S.M|S.S.A.M.M"))
        |> Array.length)
    |> Array.sum
