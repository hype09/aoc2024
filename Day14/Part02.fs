module Day14.Part02

open SkiaSharp
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

let outputImage rs iteration =   
    use bitmap = new SKBitmap(101, 103)
    bitmap.Erase(SKColors.Black)
    
    for ((x, y), _) in rs do
        bitmap.SetPixel(x, y, SKColors.White)

    use image = SKImage.FromBitmap(bitmap)
    use data = image.Encode(SKEncodedImageFormat.Png, 100)
    use fileStream = System.IO.File.OpenWrite(__SOURCE_DIRECTORY__ + $"/output/iteration{iteration.ToString().PadLeft(5, '0')}.png")
    data.SaveTo(fileStream)    

let modulo n x = ((x % n) + n) % n

let moveRobot (((x, y), (v_x, v_y)): Robot): Robot = (modulo 101 (x + v_x), modulo 103 (y + v_y)), (v_x, v_y)
    
let quad ((x, y): Position): int option =
    if x < 50 then if y < 51 then Some 0 elif y > 51 then Some 1 else None
    elif x > 50 then if y < 51 then Some 2 elif y > 51 then Some 3 else None
    else None
    
let safetyScore rs =
    List.map (fun (p,_) -> quad p) rs
    |> List.choose id
    |> List.groupBy id
    |> List.map (fun (_, occurrences) -> List.length occurrences)
    |> List.fold (*) 1
    
let solution =
    // Calculate safety scores for first 10k iterations.
    // Christmas tree -> less uniform robot distribution -> more robots in single cluster -> lower security score. 
    let interestingTimestamps =
        List.fold (fun (scores, rs) idx ->
            (safetyScore rs, idx) :: scores, List.map moveRobot rs
        ) ([], parseInput) [ 0..10000 ]
        |> fst
        |> List.sortBy fst
        |> List.take 20
        |> List.map snd
        |> List.sort
    
    // Generate images of 20 most interesting robot distributions
    List.fold (fun rs idx ->
        if List.contains idx interestingTimestamps then outputImage rs idx
        List.map moveRobot rs
    ) parseInput [ 0..10000 ]
    
    // Check manually for correct solution
    1 
    