open System.IO
open System

let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "data" , "input_5.txt")

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

let replace (oldValue: string) (newValue: string) (str: string) =
    str.Replace(oldValue, newValue)

let splitTrim (by: string) (str: string) =
    str.Split([| by |], StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)

let parseSeedIds (line: string) =
    line
    |> replace "seeds:" ""
    |> splitTrim " "
    |> Array.map uint64
    |> Array.toList

let (|MapHeader|_|) (line: string) =
    match line with
    | s when s.EndsWith("map:") ->         
        let splitValues = 
            s 
            |> replace " map:" "" 
            |> splitTrim "-to-" 
        Some (MapHeader(splitValues[0], splitValues[1]))
    | _ -> 
        None

let (|Range|_|) (line: string) =
    match line with
    | "" -> None
    | s ->
        let splitLine = s |> splitTrim " "
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
    let seedIds =
        File.ReadLines(inputPath)
        |> Seq.take(1)
        |> Seq.head        
        |> parseSeedIds

    let mapLines =
        File.ReadLines(inputPath)
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

let lowestLocation =
    almanac.SeedIds
    |> List.map (seedToLocation almanac)    
    |> List.min

printfn "Part1: %d" lowestLocation