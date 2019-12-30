// 6

let input = System.IO.File.ReadAllLines("input/input_6.txt")

type Planet = {
    Code : string
    Orbits : string
    DistanceToCenter : int
}

let (planets:Map<string, Planet>) = 
    input 
    |> Array.fold (fun planets input -> 
        let codes = input.Split ([|')'|]) 
        let parent = codes.[0]
        let orbital = codes.[1]
        planets |> Map.add orbital {Code = orbital; Orbits = parent; DistanceToCenter = 0} ) Map.empty

let findOrbitals (planets:Map<string, Planet>) code =
    planets |> Seq.filter (fun kvp -> kvp.Value.Orbits = code) |> Seq.map (fun kvp -> kvp.Value)


let calculateDistances (planets:Map<string, Planet>) = 
    let rec loop (planets:Map<string, Planet>) queue =
        match queue with
        | [] -> planets
        | planetCode :: rest -> 
            let distance = if planetCode = "COM" then 0 else planets.[planetCode].DistanceToCenter
            let orbitals = findOrbitals planets planetCode
            let planets' = 
                orbitals 
                |> Seq.fold (fun acc orbital -> 
                    acc |> Map.add orbital.Code {orbital with DistanceToCenter = distance + 1} ) planets
            let orbitalCodes = orbitals |> Seq.map (fun orbital -> orbital.Code) |> List.ofSeq
            loop planets' (orbitalCodes @ rest)

    loop planets ["COM"]

let planetsWithDistance = calculateDistances planets 

let result = planetsWithDistance |> Seq.sumBy (fun kvp -> kvp.Value.DistanceToCenter)    

let rec getParents (planets:Map<string, Planet>) code =
    if code = "COM" then [] 
    else 
        let parent = planets.[code].Orbits
        parent :: getParents planets parent

getParents planets "SAN"

let findShortestDistance list1 list2 =
    let rec loop list1 list2 distance =
        match list2 with
        | [] -> -1
        | x :: xs ->
            match list1 |> List.tryFindIndex (fun i -> i = x) with
            | Some i -> i + distance
            | None -> loop list1 xs (distance + 1)

    loop list1 list2 0        

let result2 = findShortestDistance (getParents planets "YOU") (getParents planets "SAN")   