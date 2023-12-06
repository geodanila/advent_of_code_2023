#load "common.fsx"

type Race = {
    TimePressingButton: uint64
    DistanceTravelled: uint64
}

let readInput () =
    let lines = Input.readAllLines "input_6.txt"
    match lines with
    | [| timeRow; distanceRow |] ->
        let times = 
            timeRow 
            |> String.replace "Time:" ""
            |> String.splitTrim " "
            |> Array.map uint64
            |> List.ofArray

        let distances =
            distanceRow
            |> String.replace "Distance:" ""
            |> String.splitTrim " "
            |> Array.map uint64
            |> List.ofArray
       
        List.zip times distances
        |> List.map (fun (t,d) -> { TimePressingButton = t; DistanceTravelled = d })
    | _ -> failwith "Invalid input file!"

let computeRaceTimes (race: Race) =
    seq { 0UL .. race.TimePressingButton }
    |> Seq.map (fun timeHoldingBtn -> 
        let mmPerMs = timeHoldingBtn
        let remainingTime = race.TimePressingButton - timeHoldingBtn
        let travelDistance = remainingTime * mmPerMs
        (timeHoldingBtn, travelDistance)
    )

let getRecordBreakingTimes (race: Race) =
    let raceTimes = computeRaceTimes race
    raceTimes
    |> Seq.filter (fun (t, d) -> d > race.DistanceTravelled)
    |> Seq.map fst

let numberOfWaysOfWinning =
    readInput () 
    |> Seq.map getRecordBreakingTimes
    |> Seq.map (fun x -> x |> Seq.length)    
    |> Seq.fold (fun acc x -> acc * x) 1

printfn "Part1: %d" numberOfWaysOfWinning

let readInputPart2 () =
    let lines = Input.readAllLines "input_6.txt"
    match lines with
    | [| timeRow; distanceRow |] ->
        let time = 
            timeRow 
            |> String.replace "Time:" ""            
            |> String.replace " " ""
            |> String.trim
            |> uint64

        let distance =
            distanceRow
            |> String.replace "Distance:" ""
            |> String.replace " " ""
            |> String.trim
            |> uint64
       
        { TimePressingButton = time; DistanceTravelled = distance }        
    | _ -> failwith "Invalid input file!"

let numberOfWaysOfWinningPart2 =
    readInputPart2 ()
    |> getRecordBreakingTimes
    |> Seq.length

printfn "Part2: %d" numberOfWaysOfWinningPart2
