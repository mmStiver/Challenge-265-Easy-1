let rec insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

let rec permutations = function
    | []      -> seq [ [] ]
    | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs)) 

let rec combination_r acc size set = 
    seq { match size, set with 
            | n, x::xs -> 
                if n > 0 then yield! combination_r (x::acc) (n - 1) xs
                if n >= 0 then yield! combination_r acc n xs 
            | 0, [] -> yield acc 
            | _, [] -> () }

let combinations size set =
    combination_r [] size set

[<EntryPoint>]
let main argv = 
    permutations [ 0 .. 5 ]
        |> Seq.sortBy (fun x -> (x.[0],x.[1],x.[2],x.[3]))
        |> Seq.skip 239
        |> Seq.take 1
        |> Seq.toList
        |> printfn "%A"

    permutations [ 0 .. 6 ]
        |> Seq.sortBy (fun x -> (x.[0],x.[1],x.[2],x.[3],x.[4]))
        |> Seq.skip 3239
        |> Seq.take 1
        |> Seq.toList
        |> printfn "%A"

    combinations 3 [0 .. 7]
        |> Seq.map(fun com -> List.sort com)
        |> Seq.skip 23
        |> Seq.take 1
        |> Seq.toList
        |> printfn "%A"

    combinations 4 [0 .. 8]
        |> Seq.map(fun com -> List.sort com)
        |> Seq.skip 111
        |> Seq.take 1
        |> Seq.toList
        |> printfn "%A"

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
