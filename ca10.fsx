// 10

open Microsoft.FSharp.Collections


let input2 = @".#..#
.....
#####
....#
...##"

let toStringList file = file |> Array.map (fun str -> str |> Seq.map (fun c -> c.ToString()) |> List.ofSeq ) |> List.ofArray 
let toArr (strList:string list list) = 
    let height = strList.Length
    let width = strList.[0].Length
    Array2D.init width height (fun x y -> strList.[y].[x]) 

let input = 
    System.IO.File.ReadAllLines("input/input_10.txt") 


let arr = input |> toStringList |> toArr
let arr2 = input2.Split ([|'\n'|]) |> toStringList |> toArr

arr2.[0,3]

let isAsteroidAtPos posx posy (arr:string[,]) =
    let w = arr |> Array2D.base1
    let h = arr |> Array2D.base2
    if posx < 0 || posy < 0 || posx > w || posy > h then false else arr.[posx, posy] = "#" 

let isThereAsteroid (arr:string [,]) (leftx, lefty) (vectx, vecty) max =
    [1..max] |> List.fold (fun acc i -> if isAsteroidAtPos (leftx + vectx * i) (lefty + vecty * i) arr then true else acc) false

let blocks (arr: string[,]) (pos1:int*int) (pos2:int*int) =
    let (x1,y1) = pos1
    let (x2,y2) = pos2
    if x1 = x2 then // on same x axis
        let (miny, maxy) = if y1 < y2 then (y1,y2) else (y2,y1)
        if maxy - miny < 2 then false else 
            [miny+1..maxy-1] |> List.fold (fun acc i -> if isAsteroidAtPos x1 i arr then true else acc) false
    else 
    if y1 = y2 then // on same y axis
        let (minx, maxx) = if x1 < x2 then (x1,x2) else (x2,x1)
        if maxx - minx < 2 then false else 
            [minx+1..maxx-1] |> List.fold (fun acc i -> if isAsteroidAtPos i y1 arr then true else acc) false
    else 

    let (left, right) = if x1 < x2 then (pos1, pos2) else (pos2,pos1)
    let (leftx, lefty) = left
    let (rightx, righty) = right
    let distx = rightx - leftx
    let disty = righty - lefty
    match distx with 
    | 1 -> false
    | distx -> 
        [2..(distx/2)] 
        |> List.fold (fun acc i -> 
            if distx % i = 0 && disty % i = 0 && isThereAsteroid arr left (distx, disty) (distx/i) then true else acc) false

let countVisibleAsteroids (arr:string [,])  posx posy =
    let mutable sum = 0
    if isAsteroidAtPos posx posy arr then 
        arr |> Array2D.iteri (fun x y item -> 
            if item = "#" && (posx <> x || posy <> y) && (not (blocks arr (x,y) (posx,posy))) then 
                sum <- sum + 1)
        sum        
    else 0    

let countVisible (arr:string [,]) =
    arr |> Array2D.mapi (fun x y item -> (x,y, countVisibleAsteroids arr x y))

countVisible arr2    