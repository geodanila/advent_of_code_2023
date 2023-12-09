#load "common.fsx"
open System.Collections.Generic

let readSampleInput () =     
    """
    0 3 6 9 12 15
    1 3 6 10 15 21
    10 13 16 21 30 45
    """
    |> String.splitTrim "\n"

let readInput () =
    Input.readAllLines "input_9.txt"

let processInput lines =
    lines
    |> Array.map (fun line -> 
        line
        |> String.splitTrim " "
        |> Array.map int
    )

type Direction = 
    | Forward
    | Backward

let extrapolate (direction: Direction) (numbers: int array) =
    let getDiffs numbers =
        numbers
        |> Array.pairwise
        |> Array.map (fun (a, b) -> b - a)

    let getAllDiffs numbers =    
        let collector = List<int array>()
        let rec getAllDiffs numbers (collector: List<int array>) =
            let diffs = getDiffs numbers
            collector.Add(diffs)
            match diffs |> Array.forall ((=) 0) with
            | false -> getAllDiffs diffs collector
            | true -> ()

        getAllDiffs numbers collector
        collector |> Seq.toArray   

    let allNumbers = 
        let allDiffs = getAllDiffs numbers
        allDiffs |> Array.append [|numbers|]

    let extrapolated =
        match direction with
        | Forward ->
            allNumbers
            |> Array.map (fun x -> x |> Array.last)
            |> Array.rev
            |> Array.fold (fun s x -> s + x) 0
        | Backward ->
            allNumbers
            |> Array.map (fun x -> x |> Array.head)
            |> Array.rev
            |> Array.fold (fun s x -> x - s) 0
    
    extrapolated

#time
let part1 =
    readInput () 
    |> processInput 
    |> Array.map (extrapolate Forward)
    |> Array.sum
#time

printfn "Part1: %d" part1
printfn ""

#time
let part2 =
    readInput ()
    |> processInput
    |> Array.map (extrapolate Backward)
    |> Array.sum
#time

printfn "Part2: %d" part2
printfn ""