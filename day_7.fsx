#load "common.fsx"

type Card =
    | A
    | K
    | Q
    | J
    | T
    | Number of int

module Card =
    let parse (c: char) : Card =
        match c with
        | c when c >= '2' && c <= '9' -> 
            c.ToString() 
            |> int 
            |> Number
        | 'A' -> A
        | 'K' -> K
        | 'Q' -> Q
        | 'J' -> J
        | 'T' -> T
        | _ -> failwithf "Invalid card: %c" c

    let getScore card =
        match card with
        | A -> 14
        | K -> 13
        | Q -> 12
        | J -> 11
        | T -> 10
        | Number n -> n

type HandType =
    | FiveOfAKind = 7
    | FourOfAKind = 6
    | FullHouse = 5
    | ThreeOfAKind = 4
    | TwoPair = 3
    | OnePair = 2
    | HighCard = 1
    | None = 0

type Hand = Cards of Card list

module Hand =    
    let parse (str: string) : Hand =
        let cards = 
            str
            |> String.toList
            |> List.map Card.parse 
            
        match cards.Length with
        | 5 -> Cards cards
        | _ -> failwithf "Invalid hand: %A" cards

    let toString (hand: Hand) =
        let (Cards cards) = hand
        cards
        |> List.fold (fun state card -> 
            match card with
            | Number n -> state + n.ToString()
            | _ -> state + card.ToString()
        ) ""

    let nbOfCards (g: _ * _ list) = (snd g).Length

    let getHandType (hand: Hand) =
        let (Cards cards) = hand
        let groups = 
            cards 
            |> List.groupBy id 
            |> List.sortByDescending (fun g -> nbOfCards g)

        match groups with
        | _ :: [] -> 
            HandType.FiveOfAKind
        | head :: [ _ ] when nbOfCards head = 4 -> 
            HandType.FourOfAKind
        | head :: [ _ ] when nbOfCards head = 3 ->
            HandType.FullHouse
        | head :: _ when nbOfCards head = 3 ->
            HandType.ThreeOfAKind
        | [ g1; g2; _] when nbOfCards g1 = 2 && nbOfCards g2 = 2 ->
            HandType.TwoPair
        | head :: _ when nbOfCards head = 2 ->
            HandType.OnePair
        | other when other.Length = 5 ->
            HandType.HighCard
        | _ -> 
            HandType.None

    let compare (getHandType: Hand -> HandType) (getCardScore: Card -> int) (hand1: Hand) (hand2: Hand) =
        let compareSameHandType (hand1: Hand) (hand2: Hand) = 
            let (Cards cards1) = hand1
            let (Cards cards2) = hand2

            seq {
                for i in 0..4 do
                    let cardScore1 = cards1[i] |> getCardScore
                    let cardScore2 = cards2[i] |> getCardScore
                    if cardScore1 <> cardScore2 then                        
                        yield cardScore1.CompareTo(cardScore2)
                yield 0
            } |> Seq.head // no early return in F#? No problem :P

        match (getHandType hand1), (getHandType hand2) with
            | a, b when a = b ->                
                compareSameHandType hand1 hand2
            | a, b -> 
                a.CompareTo(b) // use hand type value from enum

let readInput () =
    Input.readAllLines "input_7.txt"
    |> Array.map (fun x -> 
        x |> String.splitTrim " " 
        |> function 
            | [| handStr; bidStr |] -> 
                (Hand.parse handStr, int bidStr) 
            | _ -> failwithf "Invalid input line: %A" x
    )

let checkWinnings (compare: Hand -> Hand -> int) (hands: (Hand * int) array) =
    hands
    |> Array.sortWith (fun (hand1, _) (hand2, _) -> compare hand1 hand2) // sort the hands based on their hand type
    |> Array.indexed    
    |> Array.map (fun (index, (hand, bid)) -> index + 1, hand, bid) // set the rank using the index
    |> Array.sumBy (fun (rank, _, bid) -> rank * bid)

let input = readInput ()

let part1Compare = Hand.compare Hand.getHandType Card.getScore

let part1Result =  
    input    
    |> checkWinnings part1Compare

printfn "Part1: %d" part1Result 
printfn ""

// Part 2
let part2GetCardScore (card: Card) =
    match card with
    | J -> 1 // Jokers have a score of 1 according to the specs, when comparing hands of same type
    | _ -> Card.getScore card

let part2GetHandType (hand: Hand) =
    // replace Jokers with card of highest value that occurs the most times
    let (Cards cards) = hand
    let groupedByCard =
        cards
        |> List.sortByDescending part2GetCardScore
        |> List.groupBy id
        |> List.sortByDescending Hand.nbOfCards

    let maxCardType =         
        // edge case when there are more jokers than other card types
        if groupedByCard.Head |> fst = J && groupedByCard.Length > 1 
            then groupedByCard.Tail.Head |> fst
            else groupedByCard.Head |> fst

    let newCards = cards |> List.map (function | J -> maxCardType | x -> x)
    let adjustedHand = Cards newCards
    
    // use the adjusted hand to get the hand type
    Hand.getHandType(adjustedHand)

let part2Compare = 
    Hand.compare part2GetHandType part2GetCardScore

let part2Result = 
    input    
    |> checkWinnings part2Compare

printfn "Part2: %d" part2Result
printfn ""