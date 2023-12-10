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

let tilesToString (tiles: Tiles) =
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
    | i, j when  visited.Count > 2 && tiles[i,j] = Start -> true
    | i, j when visited.Contains(i, j) -> false
    | _ -> true

let findPath (tiles: Tiles) =    
    let checkNextPosition (tiles: Tiles) i j x y direction =
        let tileFrom = tiles[i, j]
        let tileTo = tiles[x, y]
        checkNextTile tileFrom tileTo direction

    let start = 
        match tiles |> Array2D.tryFindLocation (fun x -> x = Start) with
        | None -> failwithf "Could not find starting position in tiles \n%s" (tilesToString tiles)
        | Some (i, j) -> 
            printfn "Starting position is %i, %i for tiles \n%s" i j (tilesToString tiles)
            (i, j)

    let circuit = HashSet<_>()
    let mutable current = start
    let mutable shouldCheckNext = true

    while shouldCheckNext do
        let i, j = current        
        circuit.Add(i, j) |> ignore

        //printfn "Currently at %A (%i, %i)" tiles[i,j] i j

        if isValidPosition tiles i (j + 1) circuit && checkNextPosition tiles i j i (j + 1) Right then
            //printfn "Moving from tile %A (%i, %i) to %A (%i, %i) in direction %A" tiles[i, j] i j tiles[i, j + 1] i (j + 1) Right
            current <- (i, j + 1)
        else if isValidPosition tiles i (j - 1) circuit && checkNextPosition tiles i j i (j - 1) Left then
            //printfn "Moving from tile %A (%i, %i) to %A (%i, %i) in direction %A" tiles[i, j] i j tiles[i, j - 1] i (j - 1) Left
            current <- (i, j - 1)
        else if isValidPosition tiles (i + 1) j circuit && checkNextPosition tiles i j (i + 1) j Down then
            //printfn "Moving from tile %A (%i, %i) to %A (%i, %i) in direction %A" tiles[i, j] i j tiles[i + 1, j] (i + 1) j Down
            current <- (i + 1, j)
        else if isValidPosition tiles (i - 1) j circuit && checkNextPosition tiles i j (i - 1) j Up then
            //printfn "Moving from tile %A (%i, %i) to %A (%i, %i) in direction %A" tiles[i, j] i j tiles[i - 1, j] (i - 1) j Up
            current <- (i - 1, j)
        shouldCheckNext <- current <> start

    circuit |> Seq.toList

let path =
    readInput ()
    |> parseTiles
    |> findPath

path |> printfn "Path is: %A"

let pathWithDistances =
    path
    |> List.mapi (fun index (i,j) -> 
        let position = (i, j)
        let distance = 
            if index > path.Length / 2 
            then path.Length - index 
            else index
        (position, distance)
    )

let maxDistance = 
    pathWithDistances
    |> List.maxBy snd
    |> snd

printfn "Part1: %i" maxDistance