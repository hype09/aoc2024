module Day02.Part01

let lineToIntArray (line: string) = line.Split(" ") |> Array.map int

let rec isAsc arr =
    if Array.length arr < 2 then true
    else arr.[0] < arr.[1] && isAsc arr[1..]
        
let rec isDesc arr =
    if Array.length arr < 2 then true
    else arr.[0] > arr.[1] && isDesc arr[1..]
    
let rec isInSafeInterval arr =
    if Array.length arr < 2 then true
    else
        let dist = abs(arr.[0] - arr.[1])
        dist <= 3 && isInSafeInterval arr[1..]

let solution =
    System.IO.File.ReadLines(__SOURCE_DIRECTORY__ + "/input01.txt")
    |> Seq.map lineToIntArray
    |> Seq.filter (fun arr -> isInSafeInterval arr && (isAsc arr || isDesc arr))
    |> Seq.length
