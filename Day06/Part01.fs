module Day06.Part01

type GuardMap = char array array

let solution =
    let isGuardCell (c: char) = c = 'v' || c = '^' || c = '<' || c = '>' 
    let identifyGuard (map: GuardMap) =
        let identifyGuardRow (map: GuardMap) = Array.tryFindIndex (Array.exists isGuardCell) map
        let identifyGuardCol (map: GuardMap) r = Array.tryFindIndex isGuardCell map[r]
        match identifyGuardRow map with
        | Some r -> match identifyGuardCol map r with
                     | Some c -> Some (r, c, map[r][c])
                     | None -> None
        | None -> None
    let applyChanges (changes: (int * int * char) list) (map: GuardMap) =
        for (x, y, c) in changes do
            map[x][y] <- c
    let moveUp (r, c) (map: GuardMap) =
        if r = 0 then applyChanges [(r, c, 'X')] map
        elif not (map[r-1][c] = '#') then applyChanges [(r, c, 'X'); (r-1, c, '^')] map
        else if c = map[r].Length then applyChanges [(r, c, 'X')] map else applyChanges [(r, c, 'X'); (r, c+1, '>')] map
        map
    let moveDown (r, c) (map: GuardMap) =
        if r = (map.Length - 1) then applyChanges [(r, c, 'X')] map
        elif not (map[r+1][c] = '#') then applyChanges [(r, c, 'X'); (r+1, c, 'v')] map
        else if c = 0 then applyChanges [(r, c, 'X')] map else applyChanges [(r, c, 'X'); (r, c-1, '<')] map
        map
    let moveLeft (r, c) (map: GuardMap) =
        if c = 0 then applyChanges [(r, c, 'X')] map
        elif not (map[r][c-1] = '#') then applyChanges [(r, c, 'X'); (r, c-1, '<')] map
        else if r = 0 then applyChanges [(r, c, 'X')] map else applyChanges [(r, c, 'X'); (r-1, c, '^')] map
        map
    let moveRight (r, c) (map: GuardMap) =
        if c = (map[r].Length - 1) then applyChanges [(r, c, 'X')] map
        elif not (map[r][c+1] = '#') then applyChanges [(r, c, 'X'); (r, c+1, '>')] map
        else if r = map.Length then applyChanges [(r, c, 'X')] map else applyChanges [(r, c, 'X'); (r+1, c, 'v')] map
        map
    let rec processMap (map: GuardMap) =
        match identifyGuard map with
        | Some (r, c, dir) ->
            let map' = match dir with
                        | 'v' -> moveDown (r, c) map
                        | '^' -> moveUp (r,c) map
                        | '>' -> moveRight (r,c) map
                        | '<' -> moveLeft (r,c) map
            processMap map'
        | None -> map
    let map = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input01.txt") |> Array.map Seq.toArray
    let processedMap = processMap map
    Array.map (Array.filter ((=) 'X') >> _.Length) processedMap |> Array.sum
