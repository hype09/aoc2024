module Day04.Part01

open System.Text.RegularExpressions

let solution =
    let transpose (xs: string array) =
        xs
        |> Seq.map Seq.toList
        |> Seq.toList
        |> List.transpose
        |> Seq.map (fun chars -> System.String(List.toArray chars))
        |> Seq.toArray
    let diagonalise (xs: string array) =
        let height = xs.Length
        let width = xs[0].Length
        [ for i in 0..(width + height - 2) do
          yield [
            for j in 0..i do
            let k = i - j
            if k < height && j < width then yield xs[k][j]                              
          ] |> List.toArray |> System.String 
        ]
    let countXmasInLine x = Regex.Matches(x, "XMAS").Count + Regex.Matches(x, "SAMX").Count 
    let countXmasInLines xs = Seq.map countXmasInLine xs |> Seq.sum
    let countHorizontal xs = countXmasInLines xs
    let countVertical xs = countXmasInLines (transpose xs)
    let countMainDiagonal xs = countXmasInLines (diagonalise xs)
    let countSecondaryDiagonal xs = countXmasInLines (diagonalise (Array.rev xs))
        
    let xs = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input01.txt")
    countHorizontal xs + countVertical xs + countMainDiagonal xs + countSecondaryDiagonal xs
    