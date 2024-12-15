module Day15.Part01

open System.Text.RegularExpressions

let getCol (m: char array2d) x = [ for y in 0 .. Array2D.length1 m - 1 do yield m[y, x] ]
let setCol (m: char array2d) x (col: char list) = for y in 0 .. Array2D.length1 m - 1 do m[y, x] <- col[y]
let getRow (m: char array2d) y = [ for x in 0 .. Array2D.length2 m - 1 do yield m[y, x] ]
let setRow (m: char array2d) y (col: char list) = for x in 0 .. Array2D.length2 m - 1 do m[y, x] <- col[x]

let findMatching c m =
    [ for y in 0 .. Array2D.length1 m - 1 do
          for x in 0 .. Array2D.length2 m - 1 do
            if m[y, x] = c then yield x, y ]

let robotPosition m = (findMatching '@' m)[0]

let replace (s: string) (pattern: string, replacement: string) = Regex.Replace(s, pattern, replacement)

let solution =
    let move m c =
        let rs = "(#.*)\.(O*)@(.*#)", "${1}${2}@.${3}"
        match c, (robotPosition m) with
        | '^', (rx, ry) -> setCol m rx ((replace (getCol m rx |> Seq.toArray |> System.String) rs) |> Seq.toList)
        | 'v', (rx, ry) -> setCol m rx ((replace (getCol m rx |> Seq.toArray |> Array.rev |> System.String) rs) |> Seq.rev |> Seq.toList)
        | '<', (rx, ry) -> setRow m ry ((replace (getRow m ry |> Seq.toArray |> System.String) rs) |> Seq.toList)
        | '>', (rx, ry) -> setRow m ry ((replace (getRow m ry |> Seq.toArray |> Array.rev |> System.String) rs) |> Seq.rev |> Seq.toList)
        // printMap m c
        m
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input01.txt")
    |> String.concat "\n"
    |> _.Split("\n\n")
    |> fun xs -> (xs[0].Split("\n") |> Array.map Seq.toArray |> array2D), (Seq.toArray xs[1] |> Array.filter ((<>) '\n'))
    |> fun (m, cs) -> Array.fold move m cs
    |> findMatching 'O'
    |> List.map(fun (x, y) -> 100 * y + x)
    |> List.sum