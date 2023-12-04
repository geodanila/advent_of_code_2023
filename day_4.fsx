open System.IO
open System
open System.Text.RegularExpressions
open System.Collections.Generic

type Card = {
    CardNumber: int
    WinningNumbers: int array
    MyNumbers: int array
}

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

    let myNumbers =
        splitCard[1] 
        |> split " "
        |> Array.map int
    { CardNumber = cardNb; WinningNumbers = winningNumbers; MyNumbers = myNumbers }

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
    |> Array.toList    

let total =
    cards
    |> List.map (fun x -> (x.CardNumber, computeScore x))
    |> List.sumBy snd
printfn "Part1: %d" total

// Part 2
let processTotal () =
    let mutable counters = Dictionary<int, int>()
    let maxNb = cards |> Seq.map _.CardNumber |> Seq.max
    
    for i in 1 .. maxNb do
        counters.Add(i, 0)

    let rec processInternal (cardList: Card list) =         
        match cardList with
        | head :: tail -> 
            counters[head.CardNumber] <- counters[head.CardNumber] + 1
                
            let winnings = getMatches head |> Array.length
            let copies = 
                if winnings > 0 then
                    let remainingCards = cards |> List.skipWhile (fun x -> x.CardNumber <= head.CardNumber)
                    if remainingCards.Length >= winnings 
                        then remainingCards |> List.take winnings
                        else remainingCards
                else []
            
            processInternal copies
            processInternal tail
        | _ -> ()            

    processInternal cards

    counters 
    |> Seq.map (fun x -> x.Value)
    |> Seq.sum

processTotal () 
|> printfn "Part2: %d"