module Day04.Part01

open System.Text.RegularExpressions

let solution =
    let trans = Array.map Seq.toArray >> Array.transpose >> Array.map System.String
    let diag (xs: string array) =
        let height = xs.Length
        let width = xs[0].Length
        [ for i in 0..(width + height - 2) do
          yield [
            for j in 0..i do
            let k = i - j
            if k < height && j < width then yield xs[k][j]                              
          ] |> List.toArray |> System.String 
        ] |> List.toArray
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input01.txt")
        |> fun xs -> Array.concat [for t in [id; trans; diag; Array.rev >> diag] do t xs]
        |> Seq.map (fun x -> Regex.Matches(x, "XMAS").Count + Regex.Matches(x, "SAMX").Count)
        |> Seq.sum