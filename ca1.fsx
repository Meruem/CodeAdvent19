// 1
open System

let calcFuel (mass:int) = int (Math.Floor ((decimal mass) / (decimal 3))) - 2
let rec calcFuelFuel fuel = 
    let f' = calcFuel fuel
    match f' with 
    | f when f <= 0 -> 0
    | f -> 
        f' + calcFuelFuel f'

let input = System.IO.File.ReadAllLines("input/input_1.txt") |> Array.map int
let output = input |> Array.sumBy calcFuel
let output2 = input |> Array.sumBy calcFuelFuel

let result = calcFuelFuel 100756
