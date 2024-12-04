module Day04.Part04

open System.Text.RegularExpressions

let solution =
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input01.txt")
    |> Seq.windowed 3