open Prelude
open System

let file = "input.txt"
let lines = IO.File.ReadAllText(file) |> lines
printfn "%A" <| Day1.main lines
