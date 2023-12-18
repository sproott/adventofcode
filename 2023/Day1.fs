module Day1

open Prelude
open System

let processLine (line: string) =
    let nums = line |> Seq.map (Char.ToString >> parseInt) |> Seq.choose id
    (Seq.head nums * 10) + (Seq.last nums)


let main = List.map processLine >> List.sum
