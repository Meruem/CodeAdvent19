// 2

let input2 = 
    System.IO.File.ReadAllLines("input/input_2.txt") 
    |> Array.collect (fun str -> str.Split [| ',' |] |> Array.map int )

let getResult noun verb = 
    let arr = ResizeArray<int> (input2)    

    let rec executeInstruction (pointer:int) (arr: ResizeArray<int>) =
        if pointer >= arr.Count then failwith "out of bounds"
        match arr.[pointer] with 
        | 99 -> arr
        | 1 -> 
            arr.[arr.[pointer + 3]] <- arr.[arr.[pointer + 1]] + arr.[arr.[pointer + 2]]
            executeInstruction (pointer + 4) arr
        | 2 -> 
            arr.[arr.[pointer + 3]] <- arr.[arr.[pointer + 1]] * arr.[arr.[pointer + 2]]
            executeInstruction (pointer + 4) arr
        | _ -> sprintf "unknown instruction: %d" arr.[pointer] |> failwith 

    arr.[1] <- noun
    arr.[2] <- verb

    try
        let arr' = executeInstruction 0 arr    
        arr'.[0]
    with 
    | _ ->  -1

seq {
    for n in 0..99 do
        for v in 0..99 do
            let result = getResult n v 
            if result = 19690720 then
                yield (n, v)        
                }
    |> Seq.iter (fun (n, v) -> printfn "%d" (n * 100 + v) )  