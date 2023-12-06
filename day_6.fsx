#load "common.fsx"

open System.IO

type Race = {
    TimePressingButton: int
    DistanceTravelled: int
}

let readInput () =
    let lines = Input.readAllLines "input_6.txt"
    match lines with
    | [| timeRow; distanceRow |] ->
        let times = 
            timeRow 
            |> String.replace "Time:" ""
            |> String.splitTrim " "
            |> Array.map int
            |> List.ofArray

        let distances =
            distanceRow
            |> String.replace "Distance:" ""
            |> String.splitTrim " "
            |> Array.map int
            |> List.ofArray
       
        List.zip times distances
        |> List.map (fun (t,d) -> { TimePressingButton = t; DistanceTravelled = d })
    | _ -> failwith "Invalid input file!"

let computeRaceTimes (race: Race) =
    [0 .. race.TimePressingButton]
    |> List.map (fun timeHoldingBtn -> 
        let mmPerMs = timeHoldingBtn
        let remainingTime = race.TimePressingButton - timeHoldingBtn
        let travelDistance = remainingTime * mmPerMs
        (timeHoldingBtn, travelDistance)
    )

let getRecordBreakingTimes (race: Race) =
    let raceTimes = computeRaceTimes race
    raceTimes
    |> List.filter (fun (t, d) -> d > race.DistanceTravelled)
    |> List.map fst

let races = readInput ()

let multiplyList list =
    List.fold (fun acc x -> acc * x) 1 list

let numberOfWaysOfWinning =
    races 
    |> List.map getRecordBreakingTimes
    |> List.map (fun x -> x.Length)    
    |> multiplyList

printfn "Part1: %d" numberOfWaysOfWinning