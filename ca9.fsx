#load "IntCode.fsx"

open CodeAvent.IntCodeProcessor

let input = 
    System.IO.File.ReadAllLines("input/input_9.txt") 
    |> Array.collect (fun str -> str.Split [| ',' |] |> Array.map  (int>>bigint) )

let input2 = 
    "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99".Split [| ',' |] |> Array.map  (int>>bigint)

let input3 = "1102,34915192,34915192,7,4,7,99,0" .Split [| ',' |] |> Array.map (int>>bigint)  

let processor = Processor.Init "A" input
let cmp =  processor |> setInput (fun () -> bigint 2) |> calculateOutput |> List.ofSeq     