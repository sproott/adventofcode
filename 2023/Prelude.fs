module Prelude

open System

let lines (str: string) =
    str.Split '\n' |> Array.filter (fun s -> s <> "") |> Array.toList

let parseInt (str: string) =
    match Int32.TryParse(str) with
    | (true, i) -> Some i
    | _ -> None

type Day = string list -> int
