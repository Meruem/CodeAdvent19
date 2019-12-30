
let input =
    System.IO.File.ReadAllLines("input/input_3.txt") 
    |> Array.map (fun line -> 
        line.Split [|','|] 
        |> Array.map (fun cmd ->
            let dir = cmd.[0]
            let value = cmd.Substring(1) |> int
            (dir, value)
        ))

let snakePath1 = input.[0] |> List.ofArray
let snakePath2 = input.[1] |> List.ofArray

let mutable (posx, posy) = (0,0)
let mutable steps = 0

let makePoints path = 
    path 
    |> List.collect (fun (dir,value) -> 
        let (points, (posx', posy')) = 
            match dir with 
            | 'R' -> ([1..value] |> List.map (fun i -> (posx + i, posy, steps + i)), (posx + value, posy))
            | 'L' -> ([1..value] |> List.map (fun i -> (posx - i, posy, steps + i)), (posx - value, posy))
            | 'D' -> ([1..value] |> List.map (fun i -> (posx, posy - i, steps + i)), (posx, posy - value))
            | 'U' -> ([1..value] |> List.map (fun i -> (posx, posy + i, steps + i)), (posx, posy + value))
            | _ -> failwith "unknown direction"
        posx <- posx'    
        posy <- posy'
        steps <- steps + value
        points)


let points1 = makePoints snakePath1

posx <- 0
posy <- 0
steps <- 0

let points2 = makePoints snakePath2


let cross =
    points1 
    |> List.map (fun (p1x, p1y, p1s) -> 
        match points2 |> List.tryFind (fun (p2x, p2y, p2s) -> p1x = p2x && p1y = p2y) with
        | None -> None
        | Some (_, _, s) -> Some (p1x, p1y, p1s + s))
    |> List.choose id    

let manhattanDist x y = abs x + abs y 

let minCross1 = cross |> List.minBy (fun (x,y, steps) -> manhattanDist x y)
let result1 = 
    let (x,y, steps) = minCross1
    manhattanDist x y

let minCross2 = cross |> List.minBy (fun (x,y, steps) -> steps)

let result2 = 
    let (x,y, steps) = minCross2
    steps
 
