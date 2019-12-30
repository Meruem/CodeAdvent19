#I __SOURCE_DIRECTORY__

namespace CodeAdvent
module Common =
    let getNumbers (num:int) =
        let rec loop num list =
            let mod10 = num % 10
            if num < 10 then num :: list
            else loop (num / 10) (mod10 :: list)
        loop num []    

