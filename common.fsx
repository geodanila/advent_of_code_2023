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

module Array2D =
    let tryFindLocation predicate (array: 'a [,]) =
        let rows = array.GetLength(0)
        let columns = array.GetLength(1)
        seq {
            for i = 0 to rows - 1 do
            for j = 0 to columns - 1 do
                if predicate array.[i, j] then
                    (i, j)
        } |> Seq.tryHead

        