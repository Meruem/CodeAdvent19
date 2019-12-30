open System.Collections.Generic
#load "IntCode.fsx"

open CodeAvent.IntCodeProcessor

let input = 
    System.IO.File.ReadAllLines("input/input_11.txt") 
    |> Array.collect (fun str -> str.Split [| ',' |] |> Array.map  (bigint.Parse) )


let processor = Processor.Init "A" input
let cmp =  processor |> setInput (fun () -> bigint 2) |> calculateOutput |> List.ofSeq     

type Robot = {
    mutable X : int
    mutable Y : int
    mutable Dir : int*int
}
with 
    member this.TurnLeft () =
        match this.Dir with 
        | (-1,0) -> this.Dir <- 0,1
        | (0,1) -> this.Dir <- 1,0
        | (1,0) -> this.Dir <- 0,-1
        | (0,-1) -> this.Dir <- -1,0
        | _ -> failwith "unexpected direction"

    member this.TurnRight () =
        match this.Dir with 
        | (-1,0) -> this.Dir <- 0,-1
        | (0,1) -> this.Dir <- -1,0
        | (1,0) -> this.Dir <- 0,1
        | (0,-1) -> this.Dir <- 1,0
        | _ -> failwith "unexpected direction"

    member this.Step () =
        let (dirx,diry) = this.Dir
        this.X <- this.X + dirx
        this.Y <- this.Y + diry

    static member Init () = 
        {
            X = 0
            Y = 0
            Dir = 0,-1
        }    

let panel = Dictionary<int*int, int> ()        
let getPanelColor x y (panel:Dictionary<int*int, int>) = if panel.ContainsKey (x,y) then panel.[(x,y)] else 1
let paintUnderRobot robot color (panel:Dictionary<int*int, int>) = 
    printfn "Painted %d, %d with %d" robot.X robot.Y color
    panel.[(robot.X, robot.Y)] <- color

let robot = Robot.Init ()

let proc = Processor.Init "Robot" input |> setInput (fun () -> bigint (getPanelColor robot.X robot.Y panel))

let rec executeRobot () =
    match proc |> getNextOutput with
    | None -> panel
    | Some output1 -> 
        paintUnderRobot robot (int output1) panel
        match proc |> getNextOutput |> Option.map int with
        | None -> ()
        | Some 0 -> robot.TurnLeft() 
        | Some 1 -> robot.TurnRight()
        | _ -> failwith "unexpected turn instruction" 
        robot.Step()
        executeRobot ()

executeRobot ()

panel.Count        