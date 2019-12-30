// 7

type Instruction = {
    Mode1st : int
    Mode2nd : int
    Mode3rd : int
    OpCode : int
}

let getNumbers (num:int) =
    let rec loop num list =
        let mod10 = num % 10
        if num < 10 then num :: list
        else loop (num / 10) (mod10 :: list)
    loop num []  

let getInstruction code =
    let numbers = getNumbers code |> List.rev
    {
        OpCode = code % 100
        Mode1st = if numbers.Length >= 3 then numbers.[2] else 0
        Mode2nd = if numbers.Length >= 4 then numbers.[3] else 0
        Mode3rd = if numbers.Length >= 5 then numbers.[4] else 0
    }

type Processor = {
    Name : string    
    Instructions : int []
    mutable ReadInput : Unit -> int
    Memory : ResizeArray<int>
    mutable Pointer : int  
}    
with static member Init name instructions = {
        Name = name 
        Instructions = instructions
        ReadInput = fun () -> failwith "no input stream configured"
        Memory = ResizeArray<int> (instructions)
        Pointer = 0} 

let setInput getInput processor = 
    processor.ReadInput <- getInput
    processor         

let getNextOutput (processor:Processor) = 
    let arr = processor.Memory

    let rec executeInstruction () =
        let pointer = processor.Pointer
        if pointer >= arr.Count then failwith "out of bounds"

       // printfn "pos: %d input: %A output: %A arr: %A" pointer input output (arr.GetRange(pointer, 5))

        let instruction = getInstruction arr.[pointer]    
         
        let getIndexByParameterMode position mode =
            let position' =
                match mode with 
                | 0 -> arr.[position]  
                | 1 -> position
                | _ -> failwith (sprintf "unknown mode %d at pos:%d" mode position)
            if position' >= arr.Count || position' < 0 then 
                printfn "Out of array bounds: p:%d " pointer                 
            position'    

        let getFirstParam () = getIndexByParameterMode (pointer + 1) instruction.Mode1st
        let getSecondParam () = getIndexByParameterMode (pointer + 2) instruction.Mode2nd
        let getThirdParam () = getIndexByParameterMode (pointer + 3) instruction.Mode3rd       
        
        let incPointer n = processor.Pointer <- processor.Pointer + n        
        let log s = printfn "%s, pos %d, in: %d: %s" processor.Name processor.Pointer arr.[pointer] s

        match instruction.OpCode with 
        | 99 -> None
        | 1 -> // add
            let left = arr.[getFirstParam()]    
            let right = arr.[getSecondParam()]
            let result = left + right
            log <| sprintf "Executing instruction [ADD]: writing at {%d}: %d = {%d}%d + {%d}%d"  (getThirdParam()) result (getFirstParam()) left (getSecondParam()) right 
            arr.[getThirdParam()] <- result 
            incPointer 4
            executeInstruction ()   
        | 2 ->  // multiply
            let left = arr.[getFirstParam()]    
            let right = arr.[getSecondParam()]
            let result = left * right
            log <| sprintf "Executing instruction [MUL]: writing at {%d}: %d = {%d}%d * {%d}%d"  (getThirdParam()) result (getFirstParam()) left (getSecondParam()) right 
            arr.[getThirdParam()] <- result 
            incPointer 4
            executeInstruction ()   
        | 3 -> // get input
            let input = processor.ReadInput ()
            log <| sprintf  "Executing instruction [READ]: writing input %d at {%d}" input (getFirstParam())
            arr.[getFirstParam()] <- input
            incPointer 2
            executeInstruction ()  
        | 4 -> // write to output
            log <| sprintf  "Executing instruction [WRITE]: writing to output value %d from {%d}" arr.[getFirstParam()] (getFirstParam())
            incPointer 2
            Some arr.[getFirstParam()] 
        | 5 -> // jump if non-zero
            if arr.[getFirstParam()] <> 0 then 
                log <| sprintf  "Executing instruction [JUMP-IF-TRUE], {%d}%d is not zero, jumping by {%d}%d" (getFirstParam()) arr.[getFirstParam()] (getSecondParam()) arr.[getSecondParam()]
                processor.Pointer <- arr.[getSecondParam()]
            else   
                log <| sprintf  "Executing instruction [JUMP-IF-TRUE], {%d}%d is zero, no jump" (getFirstParam()) arr.[getFirstParam()]
                incPointer 3
            executeInstruction ()   
        | 6 -> // jump if zero
            log <| sprintf  "Executing instruction [JUMP-IF-FALSE]"
            if arr.[getFirstParam()] = 0 then 
                processor.Pointer <- arr.[getSecondParam()]
            else   
                incPointer 3
            executeInstruction ()   
          | 7 -> // less then
            let result = if arr.[getFirstParam()] < arr.[getSecondParam()] then 1 else 0
            log <| sprintf  "Executing instruction [LESS]: comparing {%d}%d < {%d}%d writing %d at {%d}" (getFirstParam()) arr.[getFirstParam()] (getSecondParam()) arr.[getSecondParam()] result (getThirdParam())
            arr.[getThirdParam()] <- result
            incPointer 4
            executeInstruction ()          
        | 8 -> // equals
            log <| sprintf  "Executing instruction [EQL]"
            let result = if arr.[getFirstParam()] = arr.[getSecondParam()] then 1 else 0
            arr.[getThirdParam()] <- result
            incPointer 4
            executeInstruction ()          
        | _ -> sprintf "unknown instruction: %d" arr.[pointer] |> failwith 
    try
        let output = executeInstruction ()      
        output
    with 
    | e -> 
        printfn "%O" e
        failwith e.Message 

let input = 
    System.IO.File.ReadAllLines("input/input_7.txt") 
    |> Array.collect (fun str -> str.Split [| ',' |] |> Array.map int )

let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

type MaybeBuilder() =

    member this.Bind(x, f) = 
        match x with
        | None -> None
        | Some a -> f a

    member this.Return(x) = 
        Some x
   
let maybe = MaybeBuilder()

let calculate instructions (ampLevels: int list) = 
    let inputStreamA = ResizeArray<int> ([ampLevels.[0]; 0]) 
    let inputStreamB = ResizeArray<int> ([ampLevels.[1]]) 
    let inputStreamC = ResizeArray<int> ([ampLevels.[2]]) 
    let inputStreamD = ResizeArray<int> ([ampLevels.[3]]) 
    let inputStreamE = ResizeArray<int> ([ampLevels.[4]]) 

    let popItem (arr:ResizeArray<int>) = 
        let item = arr.[0]
        arr.RemoveAt (0)
        item

    let pushItem item (arr:ResizeArray<int>) = 
        arr.Add (item)        

    let ampA = Processor.Init "A" instructions |> setInput (fun () -> popItem inputStreamA)
    let ampB = Processor.Init "B" instructions |> setInput (fun () -> popItem inputStreamB)
    let ampC = Processor.Init "C" instructions |> setInput (fun () -> popItem inputStreamC)
    let ampD = Processor.Init "D" instructions |> setInput (fun () -> popItem inputStreamD)
    let ampE = Processor.Init "E" instructions |> setInput (fun () -> popItem inputStreamE)

    let rec roundtrip lastValue =
        let res = 
            maybe {
                let! outputA = ampA |> getNextOutput
                inputStreamB |> pushItem outputA
                let! outputB = ampB |> getNextOutput
                inputStreamC |> pushItem outputB
                let! outputC = ampC |> getNextOutput
                inputStreamD |> pushItem outputC
                let! outputD = ampD |> getNextOutput
                inputStreamE |> pushItem outputD
                let! outputE = ampE |> getNextOutput
                inputStreamA |> pushItem outputE
                return outputE
            }
        match res with 
        | Some value -> roundtrip value
        | None -> lastValue                
    
    roundtrip 0

let input2 = @"3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5".Split [| ',' |] |> Array.map int     

let input3 = @"3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10".Split [| ',' |] |> Array.map int     

// permute [5;6;7;8;9]
//     |> List.map (fun ampLevels -> ampLevels, calculate input ampLevels)
//     |> List.maxBy (fun (ampLevels, result) -> result)

calculate input3 [9;7;8;5;6]
//calculate input2 [9;8;7;6;5]