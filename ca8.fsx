// 8

let inputStr = System.IO.File.ReadAllLines("input/input_8.txt")
let input = inputStr.[0] |> Seq.map (fun c -> int (c.ToString())) |> List.ofSeq

let width = 25
let height = 6
let size = width * height

let rec getLayers size (input : int list) = 
    seq {
        if not (input |> List.isEmpty) then 
            yield input |> List.take size
            yield! getLayers size (input |> List.skip size)
    }

let countDigits digit list =
    list |> List.sumBy (fun item -> if item = digit then 1 else 0)

let layers = getLayers size input |> List.ofSeq 
let minLayer = 
    layers 
    |> List.minBy (countDigits 0)  



let res = (countDigits 1 minLayer) * (countDigits 2 minLayer)    
    
let combineLayers (top:int list) (bottom:int list) =
    top |> List.mapi (fun i item -> if item = 2 then bottom.[i] else item)

let toDisplayString width list =
    list 
    |> List.fold (fun (acc,i) c -> 
        let str = acc + (if c = 0 then " " else "@")
        let str' = if i % width = 0 then str + "\n" else str
        (str', i+1)) ("", 1)  
    |> fst    

let combinedLayer = 
    layers 
    |> List.reduce combineLayers 
    |> toDisplayString width  

let inputStr2 = "0222112222120000"
let input2 = inputStr2 |> Seq.map (fun c -> int (c.ToString())) |> List.ofSeq
let combinedLayers2 = 
    getLayers 4 input2 
    |> List.ofSeq
    |> List.reduce combineLayers
    |> toDisplayString 2  

printfn "%s" combinedLayer    