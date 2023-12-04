open System.IO
open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Diagnostics

let winCache = Dictionary<int, int>()

type Card = {
    CardNumber: int
    WinningNumbers: int Set
    MyNumbers: int Set
} with
    member this.Wins =
        match winCache.TryGetValue(this.CardNumber) with
        | true, cacheValue -> 
            cacheValue
        | false, _ ->
            let wins = (Set.intersect this.WinningNumbers this.MyNumbers).Count
            winCache.Add(this.CardNumber, wins)
            wins

let split (sep: string) (toSplit: string) =
    toSplit.Split([|sep|], StringSplitOptions.RemoveEmptyEntries)

let readInput () =
    let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "data" , "input_4.txt")    
    File.ReadAllLines(inputPath)

let parseCard (line: string) =
    let cardNbMatch = Regex.Match(line, "Card\s+(\d+):")
    let cardNb = cardNbMatch.Groups[1].Value |> int
    let splitCard = line |> split "|"

    let winningNumbers = 
        splitCard[0] 
        |> split ":" 
        |> (fun x -> x[1].Trim())
        |> split " "
        |> Array.map int
        |> Set.ofArray

    let myNumbers =
        splitCard[1] 
        |> split " "
        |> Array.map int
        |> Set.ofArray

    { CardNumber = cardNb; WinningNumbers = winningNumbers; MyNumbers = myNumbers }    

let rec computeScoreRec matches score =
    match matches with
    | 0 -> score
    | _ -> computeScoreRec (matches - 1) (score * 2)

let computeScore (card: Card) =    
    match card.Wins with
    | 0 -> 0
    | _ -> computeScoreRec (card.Wins - 1) 1

// Part 1
let cards = 
    readInput () 
    |> Array.map parseCard
    |> Array.toList    

let total =
    cards
    |> List.map (fun x -> (x.CardNumber, computeScore x))
    |> List.sumBy snd
printfn "Part1: %d" total

// Part 2
let processTotal () =
    let counters = Dictionary<int, int>()    
    let cardNumbers = cards |> List.map _.CardNumber
    let getWins (cardNb: int) = cards[cardNb - 1].Wins
        
    for i in 1 .. cards.Length do
        counters.Add(i, 0)

    let rec processInternal head max =        
        if head < max then
            counters[head] <- counters[head] + 1
            let winnings = getWins head
            if winnings > 0 then
                processInternal (head + 1) (head + winnings + 1)
            processInternal (head + 1) max
          
    let first = List.head cardNumbers
    let last = List.last cardNumbers + 1
    processInternal first last

    // for kvp in counters do
    //     printfn "%d: %d" kvp.Key kvp.Value

    counters 
    |> Seq.map (fun x -> x.Value)
    |> Seq.sum

processTotal () 
|> printfn "Part2: %d"
