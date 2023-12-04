open System.IO
open System
open System.Text.RegularExpressions
open System.Collections.Generic
type Card = {
    CardNumber: int
    WinningNumbers: int array
    MyNumbers: int array
}

let split (sep: string) (s: string) =
    s.Split([|sep|], StringSplitOptions.RemoveEmptyEntries)

let readInput () =
    let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "data" , "input_4.txt")    
    File.ReadAllLines(inputPath)

let parseCard (line: string) =
    let cardNbMatch = Regex.Match(line, "Card\s+(\d+):")
    let cardNb = cardNbMatch.Groups.[1].Value

    let cardNb' = int cardNb
    let splitCard = line |> split "|"

    let winningNumbers = 
        splitCard[0] |> split ":" 
        |> (fun x -> x.[1].Trim())
        |> split " "
        |> Array.map int
    let myNumbers =
        splitCard[1] |> split " "
        |> Array.map int
    { CardNumber = cardNb'; WinningNumbers = winningNumbers; MyNumbers = myNumbers }

let getMatches card =
    card.WinningNumbers     
    |> Array.filter (fun x -> card.MyNumbers |> Array.contains x)

let rec computeScoreRec matches score =
    match matches with
    | 0 -> score
    | _ -> computeScoreRec (matches - 1) (score * 2)

let computeScore card =
    let nbOfMatches = getMatches card |> Array.length
    match nbOfMatches with
    | 0 -> 0
    | _ -> computeScoreRec (nbOfMatches - 1) 1

// Part 1
let cards = 
    readInput () 
    |> Array.map parseCard    

let total =
    cards
    |> Array.map (fun x -> (x.CardNumber, computeScore x))
    |> Array.sumBy snd
printfn "Part1: %d" total

let toList (arr: 'a array) =
    new List<'a>(arr)

// Part 2
let processTotal () =
    let cardList = new List<Card>(cards)
    let mutable counters = Dictionary<int, int>()
    let maxNb = cards |> Seq.map _.CardNumber |> Seq.max
    for i in 1 .. maxNb do
        counters.Add(i, 0)

    let rec processInternal (cardList: List<Card>) = 
        match cardList.Count with
        | 0 -> ()
        | _ -> 
            let head = cardList[0]
            cardList.RemoveAt(0)
            counters[head.CardNumber] <- counters.[head.CardNumber] + 1
            
            let winnings = getMatches head |> Array.length
            let toAdd = 
                (if winnings > 0 then
                    let remainingCards = cards |> Array.skipWhile (fun x -> x.CardNumber <= head.CardNumber)
                    if remainingCards.Length >= winnings 
                        then remainingCards |> Array.take winnings
                        else remainingCards
                 else [||]) |> toList
            
            processInternal toAdd
            processInternal cardList

    processInternal cardList

    counters 
    |> Seq.map (fun x -> x.Value)
    |> Seq.sum

processTotal () 
|> printfn "Part2: %d"