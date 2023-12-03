open System.IO
open System

let readInput () =
    let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "data" , "input_1.txt")    
    File.ReadAllLines(inputPath)

let findDigit (line: seq<char>) =
    line |> Seq.find (fun c -> Char.IsDigit(c)) |> string |> int

let getFirstAndLastDigits (line: string) : (int * int) =
    let firstDigit = findDigit line    
    let lastDigit = findDigit (line |> Seq.rev)
    (firstDigit, lastDigit)

let solution = 
    readInput() 
    |> Seq.map getFirstAndLastDigits
    |> Seq.map (fun (first, last) -> first * 10 + last)
    |> Seq.sum

printfn "Solution: %d" solution
