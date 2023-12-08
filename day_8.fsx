#load "common.fsx"
open System.Diagnostics

type Direction =
    | Left
    | Right

type Instruction = {
    Source: string
    Left: string
    Right: string
}

type Instructions = Instruction list

type Input = Direction seq * Instructions

let parseDirections (str: string) =
    str.ToCharArray()
    |> Array.map (
        function
        | 'L' -> Left
        | 'R' -> Right
        | c -> failwithf "Invalid direction: %c" c)

let parseInstruction (str: string) =
    let splitByEquals = str |> String.splitTrim "="
    let source = splitByEquals[0]
    let left, right = 
        splitByEquals[1] 
        |> String.replace "(" "" 
        |> String.replace ")" "" 
        |> String.splitTrim "," 
        |> (fun x -> x[0], x[1])

    { Source = source; Left = left; Right = right }



let readInput fileName : Input =
    let lines = Input.readLines fileName
    
    let directions = 
        let basicSequence =
            lines
            |> Seq.head 
            |> parseDirections 
        Seq.initInfinite (fun i -> Array.item (i % basicSequence.Length) basicSequence)

    let instructions =
        lines
        |> Seq.skip 2
        |> Seq.map parseInstruction        
        |> Seq.toList

    directions, instructions

let findInstruction (current: Instruction) (direction: Direction) (instructions: Instruction seq) =
    match direction with
    | Left -> instructions |> Seq.find (fun x -> x.Source = current.Left)
    | Right -> instructions |> Seq.find (fun x -> x.Source = current.Right)

let countSteps start directions instructions reachedEnd =
    let mutable count = 0
    let mutable current = start

    seq {
        for direction in directions do
            count <- count + 1
            current <- findInstruction current direction instructions

            if reachedEnd current then
                yield count
    } |> Seq.head

let countStepsPart1 (input: Input) =
    let (directions, instructions) = input
    let start = 
        instructions 
        |> List.sortBy _.Source 
        |> List.head    
    
    countSteps start directions instructions (fun x -> x.Source = "ZZZ")

#time
readInput "input_8.txt"
|> countStepsPart1
|> printfn "Part1: %d"
#time
printfn ""

// Part 2...

let countStepsPart2 (input: Input) =
    let (directions, instructions) = input
    let mutable count = 0
    let startingInstructions = 
        instructions 
        |> List.filter _.Source.EndsWith("A")

    let reachedEnd instruction = instruction.Source.EndsWith "Z"

    let leastStepsForEach =
        startingInstructions
        |> List.map (fun start -> countSteps start directions instructions reachedEnd)

    //leastStepsForEach |> printfn "%A"

    let rec gcd (a: uint64) (b: uint64) =
        if b = 0UL then a
        else gcd b (a % b)
    
    let lcm (a: uint64) (b: uint64) =
        if a = 0UL || b = 0UL then 0UL
        else (a * b) / gcd a b
    
    let rec lcmList = function
        | [] -> 1UL
        | [x] -> x
        | x::xs -> lcm x (lcmList xs)

    let answer = 
        leastStepsForEach 
        |> List.map uint64 
        |> lcmList
        
    answer

#time
readInput "input_8.txt"
|> countStepsPart2
|> printfn "Part2: %d"
#time
printfn ""