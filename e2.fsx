open System
open System.Security.Cryptography

//let input = "abc"
let input = "wtnhxymk"

let md5 = MD5.Create()

//md5.ComputeHash(byteArray input)

let password (input : string) = 
    let byteArray (input : string) = 
        input.ToCharArray() 
        |> Array.toList 
        |> List.map (fun c -> byte(c)) 
        |> Array.ofList

    let hexHash (hash : byte array) = 
        hash
        |> Array.map (fun el -> System.String.Format("{0:X2}", el))
        |> Array.fold (fun acc elem -> acc + elem) ""

    let startsWithFiveZeroes (hexHash : string) =
        if hexHash.Substring(0, 5) = "00000" then true else false

    let isValid (hexHash : string) =
        if fst (Int32.TryParse(hexHash.Substring(5, 1))) then 
            hexHash |> startsWithFiveZeroes
        else false

    let isValidIndex (i : int) =
        if i <= 7 then true else false

    let getSize (map : Map<int, string>) =
        map |> Map.toList |> List.length

    let rec findPass (input : string) (index : int) (output : Map<int, string>) =
        let byteInput = (input + string index) |> byteArray
        match output with
        | output when getSize output = 8 -> output
        | output                         -> let hash = md5.ComputeHash(byteInput)
                                            let hex = hash |> hexHash
                                            if isValid hex then 
                                                 let i = Int32.Parse(hex.Substring(5, 1))
                                                 if isValidIndex i then
                                                     if output.ContainsKey(i) then
                                                        findPass input (index + 1) output
                                                     else 
                                                        let x = hex.Substring(6, 1)
                                                        findPass input (index + 1) (output.Add(i, x))
                                                 else findPass input (index + 1) output
                                            else findPass input (index + 1) output

    findPass input 0 Map.empty