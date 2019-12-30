namespace CodeAvent

open System.Collections.Generic

module IntCodeProcessor = 
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

    type Address = int    

    type Processor = {
        Name : string    
        Instructions : bigint []
        mutable ReadInput : Unit -> bigint
        Memory : Dictionary<Address, bigint>
        mutable Pointer : Address  
        mutable RelativeBase : Address
        mutable Debug : bool
    }    
    with static member Init name instructions =  
            let mem = Dictionary<Address, bigint> ()
            instructions |> Array.iteri (fun i ins -> mem.[i] <- ins)
            {
                Name = name 
                Instructions = instructions
                ReadInput = fun () -> failwith "no input stream configured"
                Memory = mem
                Pointer = 0
                RelativeBase =  0
                Debug = false } 
    
    let withLogging processor = 
        processor.Debug <- true            
        processor

    let setInput getInput processor = 
        processor.ReadInput <- getInput
        processor         

    let getNextOutput (processor:Processor) = 
        let arr = processor.Memory

        let rec executeInstruction () =
            let pointer = processor.Pointer

            let readMem address = 
                if arr.ContainsKey address then arr.[address] else bigint 0

            let instruction = getInstruction (int arr.[pointer])    
             
            let getIndexByParameterMode position mode =
                let position' =
                    match mode with 
                    | 0 -> int (readMem position)  
                    | 1 -> position
                    | 2 -> processor.RelativeBase + int (readMem position)
                    | _ -> failwith (sprintf "unknown mode %d at pos:%A" mode position)
                if position' < 0 then 
                    printfn "Out of array bounds: p:%A " pointer                 
                position'    

            let getFirstParam () = getIndexByParameterMode (pointer + 1) instruction.Mode1st
            let getSecondParam () = getIndexByParameterMode (pointer + 2) instruction.Mode2nd
            let getThirdParam () = getIndexByParameterMode (pointer + 3) instruction.Mode3rd       
            
            let incPointer n = processor.Pointer <- processor.Pointer + n        
            let log s = if processor.Debug then printfn "%s" s

            if processor.Debug then printf "%s, pos%d, in %A:" processor.Name processor.Pointer arr.[pointer]

            match instruction.OpCode with 
            | 99 -> None
            | 1 -> // add
                let left = readMem (getFirstParam())    
                let right = readMem (getSecondParam())
                let result = left + right
                log <| sprintf "Executing instruction [ADD]: writing at {%d}: %A = {%d}%A + {%d}%A"  (getThirdParam()) result (getFirstParam()) left (getSecondParam()) right 
                arr.[getThirdParam()] <- result 
                incPointer 4
                executeInstruction ()   
            | 2 ->  // multiply
                let left = readMem (getFirstParam())   
                let right = readMem (getSecondParam())
                let result = left * right
                log <| sprintf "Executing instruction [MUL]: writing at {%d}: %A = {%d}%A * {%d}%A"  (getThirdParam()) result (getFirstParam()) left (getSecondParam()) right 
                arr.[getThirdParam()] <- result 
                incPointer 4
                executeInstruction ()   
            | 3 -> // get input
                let input = processor.ReadInput ()
                log <| sprintf  "Executing instruction [READ]: writing input %A at {%d}" input (getFirstParam())
                arr.[getFirstParam()] <- input
                incPointer 2
                executeInstruction ()  
            | 4 -> // write to output
                log <| sprintf  "Executing instruction [WRITE]: writing to output value %A from {%d}" (readMem (getFirstParam())) (getFirstParam())
                incPointer 2
                Some (readMem (getFirstParam())) 
            | 5 -> // jump if non-zero
                if (readMem (getFirstParam())) <> bigint 0 then 
                    log <| sprintf  "Executing instruction [JUMP-IF-TRUE], {%d}%A is not zero, jumping by {%d}%A" (getFirstParam()) (readMem (getFirstParam())) (getSecondParam()) (readMem (getSecondParam()))
                    processor.Pointer <- int <| readMem (getSecondParam())
                else   
                    log <| sprintf  "Executing instruction [JUMP-IF-TRUE], {%d}%A is zero, no jump" (getFirstParam()) (readMem (getFirstParam()))
                    incPointer 3
                executeInstruction ()   
            | 6 -> // jump if zero
                log <| sprintf  "Executing instruction [JUMP-IF-FALSE]"
                if (readMem (getFirstParam())) = bigint 0 then 
                    processor.Pointer <- int <| readMem (getSecondParam())
                else   
                    incPointer 3
                executeInstruction ()   
            | 7 -> // less then
                let result = if (readMem (getFirstParam())) < (readMem (getSecondParam())) then bigint 1 else bigint 0
                log <| sprintf  "Executing instruction [LESS]: comparing {%d}%A < {%d}%A writing %A at {%d}" (getFirstParam()) (readMem (getFirstParam())) (getSecondParam()) (readMem (getSecondParam())) result (getThirdParam())
                arr.[getThirdParam()] <- result
                incPointer 4
                executeInstruction ()          
            | 8 -> // equals
                log <| sprintf  "Executing instruction [EQL]"
                let result = if (readMem (getFirstParam())) = (readMem (getSecondParam())) then bigint 1 else bigint 0
                arr.[getThirdParam()] <- result
                incPointer 4
                executeInstruction ()          
            | 9 -> 
                processor.RelativeBase <- processor.RelativeBase + (int <| readMem (getFirstParam()))
                log <| sprintf  "Executing instruction [SET_REL], adjusting by %d, setting to %d" (int <| readMem (getFirstParam())) processor.RelativeBase
                incPointer 2 
                executeInstruction ()          
            | _ -> sprintf "unknown instruction: %A" arr.[pointer] |> failwith 
        try
            let output = executeInstruction ()      
            output
        with 
        | e -> 
            printfn "%O" e
            failwith e.Message 

    let rec calculateOutput processor =
        seq {
            match getNextOutput processor with
            | None -> ()
            | Some res -> 
                yield res
                yield! calculateOutput processor
        }

 