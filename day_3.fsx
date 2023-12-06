#load "common.fsx"

open System.IO
open System.Text.RegularExpressions

type PartNumber = {
    Number: int
    StartX: int
    EndX: int
    Y: int
}

type Symbol = {
    Symbol: char
    X: int
    Y: int
}

type Element = PartNumber of PartNumber | Symbol of Symbol

type Schematic = {
    Width: int
    Height: int
    Elements: Element array
}

let extractElement lineIndex (m: Match) : Element =
    match System.Int32.TryParse m.Value with
    | true, number -> 
        PartNumber(
        { 
            Number = number
            StartX = m.Index
            EndX = m.Index + m.Length - 1
            Y = lineIndex
        })
    | false, _ -> 
        Symbol(
        { 
            Symbol = m.Value[0]
            X = m.Index
            Y = lineIndex
        })

let readSchematic (): Schematic =
    let lines = Input.readAllLines "input_3.txt"
    let height = lines.Length
    let width = lines[0].Length
    let elements = 
        lines
        |> Array.indexed
        |> Array.collect (fun (lineIndex, line) -> 
            Regex.Matches(line, "\d+|[^.]")
            |> Seq.map (extractElement lineIndex)
            |> Seq.toArray)
    { Width = width; Height = height; Elements = elements }

let isAdjacent (p: PartNumber) (s: Symbol) =    
    if abs (p.Y - s.Y) > 1 then false // not on adjacent lines
    else if // not on adjacent columns
        abs (p.StartX - s.X) > 1 &&  
        abs (p.EndX - s.X) > 1 
    then false
    else true

let schematic = readSchematic () 

let splitNumbersAndSymbols schematic =     
    let partNumbers = 
        schematic.Elements
        |> Array.map (function | PartNumber n -> Some n | _ -> None)
        |> Array.choose id

    let symbols =
        schematic.Elements
        |> Array.map (function | Symbol s -> Some s | _ -> None)
        |> Array.choose id

    partNumbers, symbols

let partNumbers, symbols = splitNumbersAndSymbols schematic

let validPartNumbers = 
    partNumbers 
    |> Array.filter (fun n -> Array.exists (isAdjacent n) symbols)

printfn "Part 1: %A" (validPartNumbers |> Array.sumBy _.Number)

let findGears () =
    symbols
    |> Array.filter (fun s -> s.Symbol = '*')
    |> Array.choose (fun s ->
        let adjacentNumbers = 
            partNumbers
            |> Array.filter (fun n -> isAdjacent n s)
        if adjacentNumbers.Length = 2 then Some (s, adjacentNumbers)
        else None)

let gears = findGears ()

let gearRatio (partNumbers: PartNumber array) =
    match partNumbers with 
    | [|a; b |] -> a.Number * b.Number
    | _ -> failwithf "Invalid gear ratio: %A" partNumbers

let sumOfGearRatios =
    gears 
    |> Array.map snd 
    |> Array.map gearRatio 
    |> Array.sum
printfn "Part 2: %d" sumOfGearRatios