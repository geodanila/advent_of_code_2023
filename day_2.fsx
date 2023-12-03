open System.IO
open System.Text.RegularExpressions
type Game = { id: int; red: int; green: int; blue: int }

let parseGame (line: string): Game =
    let idRegex = "Game (\d+):"
    let redRegex = "(\d+) red"
    let greenRegex = "(\d+) green"
    let blueRegex = "(\d+) blue"
    let id = Regex.Match(line, idRegex).Groups.[1].Value |> int
    let red = 
        Regex.Matches(line, redRegex)
        |> Seq.map (fun x -> x.Groups.[1].Value |> int)
        |> Seq.max
    let green = 
        Regex.Matches(line, greenRegex)
        |> Seq.map (fun x -> x.Groups.[1].Value |> int)
        |> Seq.max
    let blue =
        Regex.Matches(line, blueRegex)
        |> Seq.map (fun x -> x.Groups.[1].Value |> int)
        |> Seq.max
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

let solution = 
    readInput()
    |> Array.map parseGame
    |> Array.filter (fun game -> isGamePossible game 12 13 14)
    |> Array.sumBy _.id

printfn $"Solution: {solution}"