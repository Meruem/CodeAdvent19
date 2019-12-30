// 5
#I __SOURCE_DIRECTORY__
#load "codeadvent.fsx"

namespace CodeAdvent
module IntCodeComputer = 

    open CodeAdvent.Common

    let input = 
        System.IO.File.ReadAllLines("input/input_5.txt") 
        |> Array.collect (fun str -> str.Split [| ',' |] |> Array.map int )

    type Instruction = {
        Mode1st : int
        Mode2nd : int
        Mode3rd : int
        OpCode : int
    }

    let getInstruction code =
        let numbers = getNumbers code |> List.rev
        {
            OpCode = code % 100
            Mode1st = if numbers.Length >= 3 then numbers.[2] else 0
            Mode2nd = if numbers.Length >= 4 then numbers.[3] else 0
            Mode3rd = if numbers.Length >= 5 then numbers.[4] else 0
        }
        

    let compute (instructions:int []) (input: int list) = 
        let arr = ResizeArray<int> (instructions)    

        let rec executeInstruction (pointer:int) (arr: ResizeArray<int>) input output =
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
                    printfn "Out of array bounds: p:%d output:%A" pointer output                
                position'    

            let getFirstParam () = getIndexByParameterMode (pointer + 1) instruction.Mode1st
            let getSecondParam () = getIndexByParameterMode (pointer + 2) instruction.Mode2nd
            let getThirdParam () = getIndexByParameterMode (pointer + 3) instruction.Mode3rd       

            match instruction.OpCode with 
            | 99 -> arr, output
            | 1 -> // add
                arr.[getThirdParam()] <- arr.[getFirstParam()] + arr.[getSecondParam()]
                executeInstruction (pointer + 4) arr input output
            | 2 ->  // multiply
                arr.[getThirdParam()] <- arr.[getFirstParam()] * arr.[getSecondParam()]
                executeInstruction (pointer + 4) arr input output
            | 3 -> // get input
                match input with 
                | first :: rest  ->
                    arr.[getFirstParam()] <- first    
                    executeInstruction (pointer + 2) arr rest output
                | [] -> 
                    printfn "%d, %A" pointer output
                    failwith "no input"   
            | 4 -> // write to output
                let output' = arr.[getFirstParam()] :: output    
                executeInstruction (pointer + 2) arr input output'
            | 5 -> // jump if non-zero
                 if arr.[getFirstParam()] <> 0 then 
                    executeInstruction arr.[getSecondParam()] arr input output    
                 else   
                    executeInstruction (pointer + 3) arr input output
            | 6 -> // jump if zero
                 if arr.[getFirstParam()] = 0 then 
                    executeInstruction arr.[getSecondParam()] arr input output    
                 else   
                    executeInstruction (pointer + 3) arr input output
            | 7 -> // less then
                let result = if arr.[getFirstParam()] < arr.[getSecondParam()] then 1 else 0
                arr.[getThirdParam()] <- result
                executeInstruction (pointer + 4) arr input output       
            | 8 -> // equals
                let result = if arr.[getFirstParam()] = arr.[getSecondParam()] then 1 else 0
                arr.[getThirdParam()] <- result
                executeInstruction (pointer + 4) arr input output       
            | _ -> sprintf "unknown instruction: %d" arr.[pointer] |> failwith 

        try
            let (arr', output) = executeInstruction 0 arr input []   
            output
        with 
        | e -> 
            printfn "%O" e
            []

    //getResult input [1]    
    compute input [5]    
