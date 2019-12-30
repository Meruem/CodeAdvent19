open System.Collections.Generic
open System.Text
#load "IntCode.fsx"

open CodeAvent.IntCodeProcessor

let input = 
    System.IO.File.ReadAllLines("input/input_13.txt") 
    |> Array.collect (fun str -> str.Split [| ',' |] |> Array.map  (bigint.Parse) )

let screen = Dictionary<int*int,int> ()
let getScreenItem x y (screen:Dictionary<int*int, int>) = if screen.ContainsKey (x,y) then screen.[(x,y)] else 0    
let setTile x y id (screen:Dictionary<int*int, int>) =
    screen.[(x,y)] <- id

type Game = {
    mutable X : int
    mutable Y : int
}
with static member Init () = { X = 0; Y = 0}


let game = Game.Init ()
let proc = Processor.Init "Game" input |> withLogging |> setInput (fun () -> bigint (getScreenItem game.X game.Y screen))

let rec executeGame () =
    let o1 = proc |> getNextOutput  
    let o2 = proc |> getNextOutput  
    let o3 = proc |> getNextOutput  
    match o1, o2, o3 with
    | Some output1, Some output2, Some output3 -> 
        paintUnderRobot robot (int output1) panel
        match proc |> getNextOutput |> Option.map int with
        | None -> ()
        | Some 0 -> robot.TurnLeft() 
        | Some 1 -> robot.TurnRight()
        | _ -> failwith "unexpected turn instruction" 
        robot.Step()
        executeGame ()
    | _ -> screen    

executeGame ()