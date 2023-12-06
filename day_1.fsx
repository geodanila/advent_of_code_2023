#load "common.fsx"

open System.Text.RegularExpressions

let readInput () = Input.readAllLines "input_1.txt"

let simpleDigitRegex = @"(\d)"
let complexDigitRegex = @"(\d|one|two|three|four|five|six|seven|eight|nine)"

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

let findDigit (line: string) regex =    
    let digit = Regex.Match(line, regex).Value
    getDigitValue digit

let findLastDigit (line: string) regex =
    let lastDigit = Regex.Match(line, regex, RegexOptions.RightToLeft).Value
    getDigitValue lastDigit

let getFirstAndLastDigits regex (line: string) : (int * int) =
    let firstDigit = findDigit line regex
    let lastDigit = findLastDigit line regex
    (firstDigit, lastDigit)

// Part 1
let part1 =
    readInput() 
    |> Seq.map (getFirstAndLastDigits simpleDigitRegex)
    |> Seq.map (fun (first, last) -> first * 10 + last)
    |> Seq.sum

printfn "Part1: %d" part1

// Part 2

let part2 = 
    readInput() 
    |> Seq.map (getFirstAndLastDigits complexDigitRegex)
    |> Seq.map (fun (first, last) -> first * 10 + last)
    |> Seq.sum

printfn "Part2: %d" part2

