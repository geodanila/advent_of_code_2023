#load "common.fsx"
open System.Collections.Generic

type Tile =
    | Empty
    | Galaxy

type Space = Tile[][]

type Position = int64 * int64

let readInput () =
    Input.readAllLines "input_11.txt"    

module Space =
    let parse (lines: string array) : Space =
        lines
        |> Array.map (fun line -> 
            line.ToCharArray() 
            |> Array.map (fun c -> if c = '.' then Empty else Galaxy))        

    let print (space: Space) =
        for i = 0 to space.Length - 1 do
            for j = 0 to space[i].Length - 1 do
                match space[i][j] with
                | Empty -> printf "."
                | Galaxy -> printf "#"
            printfn ""
        printfn ""

let getPairs (arr: 'T[]) =
    let n = Array.length arr
    let mutable pairs = []

    for i = 0 to n - 2 do
        for j = i + 1 to n - 1 do
            let pair = (arr.[i], arr.[j])
            pairs <- pair :: pairs

    pairs |> Array.ofList

let findEmptyRowsAndCols (space: Space) =
    let emptyRows =
        seq {
            for i in 0..space.Length - 1 do
                if space[i] |> Array.forall ((=) Empty) then
                    yield i        
        } |> Seq.toArray
    let emptyCols = 
        seq {
            let col = space |> Array.transpose
            for j in 0..col.Length - 1 do
                if col[j] |> Array.forall ((=) Empty) then
                    yield j
        } |> Seq.toArray

    emptyRows, emptyCols

let shortestDistance (p1: Position) (p2: Position) =
    let (x1, y1) = p1
    let (x2, y2) = p2
    abs (x1 - x2) + abs (y1 - y2)

let getGalaxyPositions (space: Space) =
    seq {
        for i in 0..space.Length - 1 do
        for j in 0..space[i].Length - 1 do
        if space[i][j] = Galaxy then
            yield (i, j)
    } |> Seq.toArray    

let getExpandedGalaxyPositions (expansionRate: int) (space: Space) : Position array =
    let expandedGalaxies = HashSet<_>()    
    let emptyRows, emptyCols = findEmptyRowsAndCols space
    let galaxyPositions = getGalaxyPositions space

    for (i, j) in galaxyPositions do
        let nj = emptyCols |> Array.filter (fun index -> index < j) |> Array.length
        let j' = int64(j) + int64((expansionRate - 1) * nj)

        let ni = emptyRows |> Array.filter (fun index -> index < i) |> Array.length
        let i' = int64(i) + int64((expansionRate - 1) * ni)

        expandedGalaxies.Add((i', j')) |> ignore

    expandedGalaxies |> Seq.toArray

let space =
    readInput () 
    |> Space.parse

#time

let totalDistance =
    space    
    |> getExpandedGalaxyPositions 2
    |> getPairs
    |> Array.sumBy (fun (p1, p2) -> shortestDistance p1 p2)

#time

printfn "Part1: %d" totalDistance
printfn ""

// =============================================
// PART 2
// =============================================

#time

let totalDistance2 =
    space    
    |> getExpandedGalaxyPositions 1_000_000
    |> getPairs
    |> Array.sumBy (fun (p1, p2) -> shortestDistance p1 p2)

#time

printfn "Part2: %d" totalDistance2
printfn ""
