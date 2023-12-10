#load "common.fsx"
open System.Text
open System.Collections.Generic

module Samples =
    let sample1 = """
    .....
    .S-7.
    .|.|.
    .L-J.
    .....
    """

    let sample2 = """
    -L|F7
    7S-7|
    L|7||
    -L-J|
    L|-JF
    """

    let sample3 = """
    ..F7.
    .FJ|.
    SJ.L7
    |F--J
    LJ...
    """

    let sample4 = """
    7-F7-
    .FJ|7
    SJLL7
    |F--J
    LJ.LJ
    """

    let sample5 = """
    ...........
    .S-------7.
    .|F-----7|.
    .||.....||.
    .||.....||.
    .|L-7.F-J|.
    .|..|.|..|.
    .L--J.L--J.
    ...........
    """

    let sample6 = """
    .F----7F7F7F7F-7....
    .|F--7||||||||FJ....
    .||.FJ||||||||L7....
    FJL7L7LJLJ||LJ.L-7..
    L--J.L7...LJS7F-7L7.
    ....F-J..F7FJ|L7L7L7
    ....L7.F7||L7|.L7L7|
    .....|FJLJ|FJ|F7|.LJ
    ....FJL-7.||.||||...
    ....L---J.LJ.LJLJ...
    """

    let sample7 = """
    FF7FSF7F7F7F7F7F---7
    L|LJ||||||||||||F--J
    FL-7LJLJ||||||LJL-77
    F--JF--7||LJLJ7F7FJ-
    L---JF-JLJ.||-FJLJJ7
    |F|F-JF---7F7-L7L|7|
    |FFJF7L7F-JF7|JL---7
    7-L-JL7||F7|L7F-7F7|
    L.L7LFJ|||||FJL7||LJ
    L7JLJL-JLJLJL--JLJ.L
    """

let readSampleInput input = 
    input
    |> String.splitTrim "\n" 
    |> Array.map _.ToCharArray()

let readInput () =
    Input.readAllLines "input_10.txt"
    |> Array.map _.ToCharArray()

type Pipe =
    | Vertical
    | Horizontal
    | NE
    | NW
    | SW
    | SE

type Tile =
    | Empty
    | Start
    | Pipe of Pipe

type Tiles = Tile[,]

type Direction =
    | Right
    | Down
    | Left
    | Up

let parseTiles (input: char array array): Tiles =
    let rows = input.Length
    let cols = input[0].Length
    let mutable tiles = Array2D.zeroCreate rows cols
    
    for row in 0..rows - 1 do
        for col in 0..cols - 1 do
            let tile =
                match input[row][col] with
                | '.' -> Empty
                | 'S' -> Start
                | '|' -> Pipe Vertical
                | '-' -> Pipe Horizontal
                | 'L' -> Pipe NE
                | 'J' -> Pipe NW
                | '7' -> Pipe SW
                | 'F' -> Pipe SE                
                | c -> failwithf "Invalid tile: %c" c
            tiles[row, col] <- tile
    tiles

module Tiles =
    let toString (tiles: Tiles) =
        let builder = StringBuilder()
        for i in 0..tiles.GetLength(0) - 1 do
            for j in 0..tiles.GetLength(1) - 1 do
                match tiles[i, j] with
                | Empty -> builder.Append "." |> ignore
                | Start -> builder.Append "S" |> ignore
                | Pipe Vertical -> builder.Append "║" |> ignore
                | Pipe Horizontal -> builder.Append "═" |> ignore
                | Pipe NE -> builder.Append "╚" |> ignore
                | Pipe NW -> builder.Append "╝" |> ignore
                | Pipe SW -> builder.Append "╗" |> ignore
                | Pipe SE -> builder.Append "╔" |> ignore
            builder.Append("\n") |> ignore
        builder.ToString()

let checkNextTile (tileFrom: Tile) (tileTo: Tile) direction = 
    //printfn "checkNextTile: %A -> %A in direction %A" tileFrom tileTo direction
    
    match direction, tileFrom, tileTo with
    | _, Empty, _ -> false
    | _, _, Empty -> false
    | _, _, Start ->
        true
    | dir, Start, Pipe pipe ->
        match dir, pipe with
        | Right, (Horizontal | SW | NW) -> true
        | Down, (Vertical | NE | NW) -> true
        | Left, (Horizontal | SE | NE) -> true
        | Up, (Vertical | SW | SE) -> true
        | _ -> false    
    | Right, Pipe pipeFrom, Pipe pipeTo ->
        match pipeFrom, pipeTo with
        | (Horizontal | NE | SE), (Horizontal | NW | SW) -> true
        | _ -> false
    | Down, Pipe pipeFrom, Pipe pipeTo ->
        match pipeFrom, pipeTo with
        | (Vertical | SW | SE), (Vertical | NE | NW) -> true
        | _ -> false
    | Left, Pipe pipeFrom, Pipe pipeTo ->
        match pipeFrom, pipeTo with
        | (Horizontal | SW | NW), (Horizontal | NE | SE) -> true
        | _ -> false
    | Up, Pipe pipeFrom, Pipe pipeTo -> 
        match pipeFrom, pipeTo with
        | (Vertical | NE | NW), (Vertical | SW | SE) -> true
        | _ -> false

let isValidPosition (tiles: Tiles) i j (visited: HashSet<_>) =
    let maxRow = tiles.GetLength(0) - 1
    let maxCol = tiles.GetLength(1) - 1
    //printfn "checking %i, %i with maxRow=%i and maxCol=%i" i j maxRow maxCol
    match i, j with
    | i, j when i < 0 || i > maxRow || j < 0 || j > maxCol -> false
    | i, j when  visited.Count >= 4 && tiles[i,j] = Start -> true // need at least 4 tiles to form a loop
    | i, j when visited.Contains(i, j) -> false
    | _ -> true

let findPath (tiles: Tiles) =    
    let checkNextPosition (tiles: Tiles) i j x y direction =
        let tileFrom = tiles[i, j]
        let tileTo = tiles[x, y]
        checkNextTile tileFrom tileTo direction

    let start = 
        match tiles |> Array2D.tryFindLocation (fun x -> x = Start) with
        | None -> failwithf "Could not find starting position in tiles \n%s" (Tiles.toString tiles)
        | Some (i, j) -> 
            printfn "Starting position is %i, %i for tiles \n%s" i j (Tiles.toString tiles)
            (i, j)

    let visited = HashSet<_>()
    let mutable current = start
    let mutable shouldCheckNext = true

    while shouldCheckNext do
        let i, j = current        
        visited.Add(i, j) |> ignore

        //printfn "Currently at %A (%i, %i)" tiles[i,j] i j

        if isValidPosition tiles i (j + 1) visited && checkNextPosition tiles i j i (j + 1) Right then
            //printfn "Moving from tile %A (%i, %i) to %A (%i, %i) in direction %A" tiles[i, j] i j tiles[i, j + 1] i (j + 1) Right
            current <- (i, j + 1)
        else if isValidPosition tiles i (j - 1) visited && checkNextPosition tiles i j i (j - 1) Left then
            //printfn "Moving from tile %A (%i, %i) to %A (%i, %i) in direction %A" tiles[i, j] i j tiles[i, j - 1] i (j - 1) Left
            current <- (i, j - 1)
        else if isValidPosition tiles (i + 1) j visited && checkNextPosition tiles i j (i + 1) j Down then
            //printfn "Moving from tile %A (%i, %i) to %A (%i, %i) in direction %A" tiles[i, j] i j tiles[i + 1, j] (i + 1) j Down
            current <- (i + 1, j)
        else if isValidPosition tiles (i - 1) j visited && checkNextPosition tiles i j (i - 1) j Up then
            //printfn "Moving from tile %A (%i, %i) to %A (%i, %i) in direction %A" tiles[i, j] i j tiles[i - 1, j] (i - 1) j Up
            current <- (i - 1, j)
        shouldCheckNext <- current <> start

    visited |> List.ofSeq

let tiles = 
    //readSampleInput Samples.sample7
    readInput ()
    |> parseTiles

let path = tiles |> findPath
path |> printfn "Path is: %A"

let pathWithDistances =
    path
    |> List.mapi (fun index (i,j) -> 
        let position = (i, j)
        let distance = 
            if index > path.Length / 2 
            then path.Length - index 
            else index
        (position, distance))

let maxDistance = 
    pathWithDistances
    |> List.maxBy snd
    |> snd

printfn "Part1: %i" maxDistance

// Part 2...
let countEnclosedTiles (tiles: Tiles) (path: (int * int) list) =
    let maxRow = tiles.GetLength(0) - 1
    let maxCol = tiles.GetLength(1) - 1
    let mutable count = 0

    for i in 0..maxRow do
        let mutable above = false
        for j in 0..maxCol do
            match tiles[i, j], path |> Seq.contains (i, j) with 
            | (Pipe Vertical | Pipe NE | Pipe NW), true -> 
                above <- not above
            | _ -> ()

            if above && not (path |> Seq.contains (i, j)) then
                count <- count + 1
    count

let nbOfEnclosedTiles =
    countEnclosedTiles tiles path

printfn "Part2: %i" nbOfEnclosedTiles