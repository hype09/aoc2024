module Day07.Part01

open System;

let parseLine (l: string) =
    l.Split(':')
    |> fun ls -> (uint64 ls[0], ls[1].Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map uint64 |> Array.toList)

let rec validate (target: uint64) (res: uint64) (vals: uint64 list) =
    match vals with
    | [v] -> target = (res + v) || target = (res * v)
    | v::vs -> validate target (res + v) vs || validate target (res * v) vs

let solution =
    System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "/input01.txt")
    |> Seq.map parseLine
    |> Seq.filter (fun (res, v::vs) -> validate res v vs)
    |> Seq.map fst
    |> Seq.sum