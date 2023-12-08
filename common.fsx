[<AutoOpen>]
module Common

open System
open System.IO

module String =
    let replace (oldValue: string) (newValue: string) (str: string) =
        str.Replace(oldValue, newValue)

    let splitTrim (by: string) (str: string) =
        str.Split([| by |], StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)

    let trim (str: string) =
        str.Trim()

    let toList (str: string) : char list =
        seq {
            for c in str do
                yield c
        } |> Seq.toList

module Input =    
    let readAllLines fileName =
        let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "data" , fileName)
        File.ReadAllLines(inputPath)

    let readLines fileName =
        let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "data" , fileName)
        File.ReadLines(inputPath)

