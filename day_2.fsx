open System.IO
open System.Text.RegularExpressions
type Game = { id: int; red: int; green: int; blue: int }

let idRegex = "Game (\d+):"
let redRegex = "(\d+) red"
let greenRegex = "(\d+) green"
let blueRegex = "(\d+) blue"

let extractMaxValue line regex =
    Regex.Matches(line, regex)
    |> Seq.map (fun x -> x.Groups.[1].Value |> int)
    |> Seq.max

let parseGame (line: string): Game =    
    let id = Regex.Match(line, idRegex).Groups.[1].Value |> int
    let red = extractMaxValue line redRegex
    let green = extractMaxValue line greenRegex
    let blue = extractMaxValue line blueRegex
    { id = id; red = red; green = green; blue = blue}

let readInput () =
    let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "data" , "input_2.txt")    
    File.ReadAllLines(inputPath)    

let isGamePossible (game: Game) red green blue =    
    let isPossible = game.red <= red && game.green <= green && game.blue <= blue
    if isPossible then
        printfn $"Game {game.id} is possible because totalRed = {game.red} <= {red}, totalGreen = {game.green} <= {green}, totalBlue = {game.blue} <= {blue}" 
        true
    else
        printfn $"Game {game.id} is not possible because totalRed = {game.red}, totalGreen = {game.green}, totalBlue = {game.blue}" 
        false

let allGames =
    readInput()
    |> Array.map parseGame

let possibleGames = 
    allGames
    |> Array.filter (fun game -> isGamePossible game 12 13 14)

// Part 1
let part1 = 
    possibleGames
    |> Array.sumBy _.id

printfn $"Part1: {part1}"

// Part 2
let computePowerOfGame (game: Game) =
    let power = game.red * game.green * game.blue
    //printfn "Power of game %A is %d" game power
    power

let part2 =
    allGames
    |> Array.map computePowerOfGame
    |> Array.sum
printfn $"Part2: {part2}"