open System.IO
open System
open System.Text.RegularExpressions

let readInput () =
    let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "data" , "input_1.txt")    
    File.ReadAllLines(inputPath)

let digitRegex = @"(\d|one|two|three|four|five|six|seven|eight|nine)"

let getDigitValue (digit: string) =
    match digit.ToLower() with
    | "one" -> 1
    | "two" -> 2
    | "three" -> 3
    | "four" -> 4
    | "five" -> 5
    | "six" -> 6
    | "seven" -> 7
    | "eight" -> 8
    | "nine" -> 9
    | _ -> digit |> int

let findDigit (line: string) =    
    let digit = Regex.Match(line, digitRegex).Value
    getDigitValue digit    

let findLastDigit (line: string) =
    let lastDigit = Regex.Match(line, digitRegex, RegexOptions.RightToLeft).Value
    getDigitValue lastDigit

let getFirstAndLastDigits (line: string) : (int * int) =
    let firstDigit = findDigit line    
    let lastDigit = findLastDigit line
    (firstDigit, lastDigit)

let solution = 
    readInput() 
    |> Seq.map getFirstAndLastDigits
    |> Seq.map (fun (first, last) -> first * 10 + last)
    |> Seq.sum

printfn "Solution: %d" solution

