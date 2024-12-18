module Day17.Part01

type Operand = Literal of int | Combo of int
type Opcode = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv
type State = (int * int * int) * int

let instruction (x: int) (y: int) (a,b, c) =
    let operand (o: Operand): int =
        match o with
        | Literal x -> x
        | Combo x when x >= 0 && x <= 3 -> x
        | Combo 4 -> a
        | Combo 5 -> b
        | Combo 6 -> c
        | Combo err -> failwith $"Unsupported combo operand: {err}"
    match x with
    | 0 -> Adv, operand (Combo y)
    | 1 -> Bxl, operand (Literal y)
    | 2 -> Bst, operand (Combo y)
    | 3 -> Jnz, operand (Literal y)
    | 4 -> Bxc, operand (Literal y)
    | 5 -> Out, operand (Combo y)
    | 6 -> Bdv, operand (Combo y)
    | 7 -> Cdv, operand (Combo y)
    | _ -> failwith $"Unsupported opcode: {x}"

let program = [| 2; 4; 1; 5; 7; 5; 4; 3; 1; 6; 0; 3; 5; 5; 3; 0 |]

let run (a, b, c) =
    ((a, b, c), 0)
    |> Seq.unfold (fun (((a, b, c), ip): State) ->
        if ip > program.Length - 1 then None
        else match instruction program[ip] program[ip + 1] (a, b, c) with
             | Adv, y -> Some ("", (((a / pown 2 y), b, c), (ip + 2)))
             | Bxl, y -> Some ("", ((a, (b ^^^ y), c), (ip + 2)))
             | Bst, y -> Some ("", ((a, (y % 8), c), (ip + 2)))
             | Jnz, y -> if a = 0 then Some ("", ((a, b, c), (ip + 2))) else Some ("", ((a, b, c), y))
             | Bxc, _ -> Some ("", ((a, (b ^^^ c), c), (ip + 2)))
             | Out, y -> Some (string (y % 8), ((a, b, c), (ip + 2)))
             | Bdv, y -> Some ("", ((a, (a / pown 2 y), c), (ip + 2)))
             | Cdv, y -> Some ("", ((a, b, (a / pown 2 y)), (ip + 2)))
    )
    |> Seq.filter ((<>) "")
    |> String.concat ","

let solution = run (61156655, 0, 0)