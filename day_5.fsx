#load "common.fsx"

open System.IO
open System

type Range = {
    DestinationRangeStart: uint64
    SourceRangeStart: uint64
    RangeLength: uint64
}

type Map = {
    Source: string
    Destination: string
    Ranges: Range list
}

type Almanac = {
    SeedIds: uint64 list
    Maps: Map list
}

type ParseState = {
    CurrentMap: Map option
    Maps: Map list
}

let parseSeedIds (line: string) =
    line
    |> String.replace "seeds:" ""
    |> String.splitTrim " "
    |> Array.map uint64
    |> Array.toList

let (|MapHeader|_|) (line: string) =
    match line with
    | s when s.EndsWith("map:") ->         
        let splitValues = 
            s 
            |> String.replace " map:" "" 
            |> String.splitTrim "-to-" 
        Some (MapHeader(splitValues[0], splitValues[1]))
    | _ -> 
        None

let (|Range|_|) (line: string) =
    match line with
    | "" -> None
    | s ->
        let splitLine = s |> String.splitTrim " "
        if splitLine.Length <> 3 then
            None
        else
            let values = splitLine |> Array.map uint64
            Some { 
                DestinationRangeStart = values[0] 
                SourceRangeStart = values[1] 
                RangeLength = values[2] 
            }

let readAlamanac () = 
    let lines = Input.readLines "input_5.txt"

    let seedIds =
        lines
        |> Seq.take(1)
        |> Seq.head        
        |> parseSeedIds

    let mapLines =
        lines
        |> Seq.skip(2)

    let initialState = {  
        CurrentMap = None;
        Maps = []
    }

    let foldedState =          
        Seq.fold (fun state line ->
            match line with
            | MapHeader (source, destination) ->             
                { state with 
                    CurrentMap = Some { 
                        Source = source 
                        Destination = destination 
                        Ranges = [] 
                    } 
                }
            | "" -> 
                match state.CurrentMap with
                | Some map ->
                    { state with 
                        CurrentMap = None
                        Maps = map :: state.Maps
                    }
                | _ -> state
            | Range r ->                
                match state.CurrentMap with
                | Some map -> 
                    { state with
                        CurrentMap = Some { map with  Ranges = r :: map.Ranges }
                    }
                | _ -> state
            | _ -> state
        ) initialState mapLines    

    // make sure all maps have been completed
    let maps = 
        match foldedState.CurrentMap with
        | None -> foldedState.Maps
        | Some unfinished -> unfinished :: foldedState.Maps

    { SeedIds = seedIds; Maps = maps }

let applyMap (map: Map) (source: uint64) =
    map.Ranges
    |> List.tryFind (fun range -> source >= range.SourceRangeStart && source < range.SourceRangeStart + range.RangeLength)
    |> Option.map (fun range -> range.DestinationRangeStart - range.SourceRangeStart + source)
    |> Option.defaultValue source

let applyReverseMap (map: Map) (destination: uint64) =
    map.Ranges
    |> List.tryFind (fun range -> destination >= range.DestinationRangeStart && destination < range.DestinationRangeStart + range.RangeLength)
    |> Option.map (fun range -> range.SourceRangeStart - range.DestinationRangeStart + destination)
    |> Option.defaultValue destination

let seedToLocation (almanac: Almanac) (seed: uint64) =
    let findMap source =
        almanac.Maps
        |> List.tryFind (fun x -> x.Source = source)

    let mutable source = "seed"
    let mutable current = seed

    while source <> "" do
        let map = findMap source
        match map with
        | Some map -> 
            //printf "Applying map %s-to-%s on current %d" map.Source map.Destination current
            current <- applyMap map current
            //printfn " => %d" current
            source <- map.Destination
        | None -> source <- ""

    current
    
let almanac = readAlamanac ()

let findLowestLocation () =
    almanac.SeedIds
    |> List.map (seedToLocation almanac)    
    |> List.min

findLowestLocation ()
|> printfn "Part1: %d"

let locationToSeed (almanac: Almanac) (location: uint64) =
    let findMap destination =
        almanac.Maps        
        |> List.tryFind (fun x -> x.Destination = destination)

    let mutable destination = "location"
    let mutable current = location

    while destination <> "" do
        let map = findMap destination
        match map with
        | Some map -> 
            //printf "Applying reverse map %s-to-%s on current %d" map.Destination map.Source current
            current <- applyReverseMap map current
            //printfn " => %d" current
            destination <- map.Source
        | None -> destination <- ""

    current

let findLowestLocationWithRanges () =
    let seedIds =
        almanac.SeedIds
        |> List.pairwise
        |> List.indexed
        |> List.filter (fun (i, _) -> i % 2 = 0)
        |> List.map snd

    let isInitialSeed seed =
        seedIds
        |> List.exists (fun (start, range) -> seed >= start && seed < start + range)        

    let found =
        seq {
            for location in seq { 0UL .. UInt64.MaxValue } do
                let seed = locationToSeed almanac location
                yield (location, seed)
        } 
        |> Seq.tryFind (fun (location, seed) -> isInitialSeed seed)
        
    match found with
    | Some (location, seed) -> 
        printfn "Lowest location is %d corresponding to seed %d" location seed
        location
    | None -> failwith "Could not determine minimum location number!"

findLowestLocationWithRanges()
|> printfn "Part2: %d"