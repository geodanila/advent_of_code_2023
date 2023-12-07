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
            } |> Seq.head

        match (getHandType hand1), (getHandType hand2) with
            | a, b when a = b ->                
                let result = compareSameHandType hand1 hand2
                // printfn "Comparing 2 hands of same type %A %A <-> %A => %d" a (toString hand1) (toString hand2) result
                result
            | a, b -> 
                a.CompareTo(b)

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
    |> Array.sortWith (fun (hand1, _) (hand2, _) -> compare hand1 hand2)
    |> Array.indexed // set the rank using the index    
    |> Array.map (fun (rank, (hand, bid)) -> 
        //printfn "Hand: %s, Rank: %d" (Hand.toString hand) (rank + 1)
        (rank, hand, bid))
    |> Array.sumBy (fun (rank, _, bid) -> (rank + 1) * bid)

printfn "============ PART1 ============"
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
    | J -> 1
    | _ -> Card.getScore card

let part2GetHandType (hand: Hand) =
    let (Cards cards) = hand
    let maxCardType = 
        cards
        |> List.sortByDescending (fun x -> part2GetCardScore x)
        |> List.groupBy id         
        |> List.sortByDescending (fun g -> Hand.nbOfCards g)
        |> (fun x -> 
            if x.Head |> fst = J && x.Length > 1 
                then x.Tail
                else x
        )
        //|> (fun x -> printfn "Hand %s has max card type %A" (Hand.toString hand) (x.Head |> fst); x)
        |> List.head
        |> fst
    let newCards = cards |> List.map (function | J -> maxCardType | x -> x)
    let adjustedHand = Cards newCards
    
    // if adjustedHand <> hand then
    //     printfn "Adjusted hand from %s to %s" (Hand.toString hand) (Hand.toString adjustedHand)

    Hand.getHandType(adjustedHand)

printfn "============ PART2 ============"

let part2Compare = 
    Hand.compare part2GetHandType part2GetCardScore

let part2Result = 
    input    
    |> checkWinnings part2Compare

printfn "Part2: %d" part2Result
printfn ""