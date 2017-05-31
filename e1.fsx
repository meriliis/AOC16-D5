open System
open System.Security.Cryptography

let input = "abc"
//let input = "wtnhxymk"

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

    let rec findPass (input : string) (index : int) (output : string) =
        let byteInput = (input + string index) |> byteArray
        match output with
        | output when output.Length = 8 -> output
        | output                        -> let hash = md5.ComputeHash(byteInput)
                                           let hex = hash |> hexHash
                                           if startsWithFiveZeroes hex then 
                                                let x = hex.Substring(5, 1)
                                                findPass input (index + 1) (output + x)
                                           else findPass input (index + 1) output

    findPass input 0 ""