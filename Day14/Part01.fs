module Day14.Part01

open System.Text.RegularExpressions

type Position = int * int
type Velocity = int * int
type Robot = Position * Velocity
type Robots = List<Position * Velocity>

let parseInput: Robots =
    System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "/input01.txt")
    |> Seq.map (
        fun l -> Regex.Match(l, "p=(\d+),(\d+) v=(-?\d+),(-?\d+)")
        >> fun m -> [for i in [1..4] do yield m.Groups[i].Value]
        >> List.map int
        >> fun ns -> (ns[0], ns[1]), (ns[2], ns[3])
    )
    |> Seq.toList

let modulo n x = ((x % n) + n) % n

let moveRobot (((x, y), (v_x, v_y)): Robot): Robot = (modulo 101 (x + v_x), modulo 103 (y + v_y)), (v_x, v_y)
    
let quad ((x, y): Position): int option =
    if x < 50 then if y < 51 then Some 0 elif y > 51 then Some 1 else None
    elif x > 50 then if y < 51 then Some 2 elif y > 51 then Some 3 else None
    else None
    
let solution =
    List.fold (fun rs _ -> List.map moveRobot rs) parseInput [ 1..100 ]
    |> List.map (fun (p,_) -> quad p)
    |> List.choose id
    |> List.groupBy id
    |> List.map (fun (_, occurrences) -> List.length occurrences)
    |> List.fold (*) 1
