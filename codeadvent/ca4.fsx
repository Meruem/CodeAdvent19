let min = 152085
let max = 670283


let numbers (num:int) =
    let rec loop num list =
        let mod10 = num % 10
        if num < 10 then num :: list
        else loop (num / 10) (mod10 :: list)
    loop num []    

let countNumbers list =
    list
    |> List.fold (fun map n -> 
        match map |> Map.tryFind n with 
        | None -> map |> Map.add n 0
        | Some x -> map |> Map.add n (x+1)) Map.empty 

let hasDouble list =
    list |> countNumbers |> Map.exists (fun k value -> value = 2)          


let rec areSiblings list = 
    match list with
    | a :: b :: rest -> a = b || areSiblings (b :: rest)    
    | _ -> false

let rec areStrictSiblings list = 
    match list |> List.fold (fun (n, count, succ) i -> if i = n then (i, count + 1, succ) else (i, 1, succ || count = 2)) (-1, 0, false) with
    | (_, _, true) -> true
    | (_, 2, false) -> true
    | _ -> false

areStrictSiblings [1;2;3;1;1;4]    

let rec isNotDescending list =    
    match list with 
    | a :: b :: rest -> a <= b && isNotDescending (b :: rest) 
    | _ -> true

[min..max] 
    |> List.sumBy (fun num ->
        let nums = numbers num
        if areSiblings nums && isNotDescending nums then 1 else 0       
    )

[min..max] 
    |> List.sumBy (fun num ->
        let nums = numbers num
        if areStrictSiblings nums && isNotDescending nums then 1 else 0       
    )    