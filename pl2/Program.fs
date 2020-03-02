// Learn more about F# at http://fsharp.org

open System

type Pole<'a> = Pole of int * list<'a>

module List =
    let skip n list =
        if n > List.length list
        then []
        else List.skip n list



let getNext (poles:(int * int list) list) = seq {
    for source in poles do
        for sink in poles |> Seq.except [source] do
            match source, sink with
            | (i,h1::t1), (j,h2::t2) when h2 > h1->
                yield 
                    if i < j
                    then (List.take i poles) @ [i,t1] @ (poles |> List.skip (i+1) |> List.take (j-i-1)) @ [j,h1::h2::t2] @ (List.skip (j+1) poles)
                    else (List.take j poles) @ [j,h1::h2::t2] @ (poles |> List.skip (j+1) |> List.take (i-j-1)) @ [i,t1] @ (List.skip (i+1) poles)
            | (i,h1::t1), (j,[]) ->
                yield
                    if i < j
                    then (List.take i poles) @ [i,t1] @ (poles |> List.skip (i+1) |> List.take (j-i-1)) @ [j,[h1]] @ (List.skip (j+1) poles)
                    else (List.take j poles) @ [j,[h1]] @ (poles |> List.skip (j+1) |> List.take (i-j-1)) @ [i,t1] @ (List.skip (i+1) poles)
            | _ -> ()
}


type Configuration = (int*int list) list
type Path = Configuration list

// Head recursion
let findPathDiveFirstHead (poles : Configuration) (desiredState : Configuration) : Path option =
    let rec inFunc left path : Path option =
        match left with
        | [] -> 
            None
        | h::_ when h = desiredState ->
           h::path |> Some
        | h::t when List.contains h path ->
            inFunc t path 
        | h::t ->
            let nextCombinations = getNext h |> Seq.toList
            match inFunc nextCombinations (h::path) with
            | None -> inFunc t path
            | x -> x

    inFunc [poles] []



// Tail recursion
let findPathDiveFirst (startState : Configuration) (desiredState : Configuration) : Path option =
    let rec inFunc left path next : Path option =
        match left with
        | [] -> 
           None |> next
        | h::_ when h = desiredState ->
           h::path |> Some |> next
        | h::t when List.contains h path ->
            inFunc t path next 
        | h::t ->
            let nextCombinations = getNext h |> Seq.toList
            inFunc nextCombinations (h::path) (function 
                | None -> inFunc t path next 
                | x -> x |> next)

    inFunc [startState] [] id

let findPathBreadthFirst (startState : Configuration) (desiredState : Configuration) : Path option =
    let rec inFunc left allPassed allDiscovered path next : Path option =
        match left with
        | [] -> 
            None |> next allPassed allDiscovered
        | h::_ when h = desiredState ->
            h::path |> Some |> next allPassed allDiscovered
        | h::t when  List.contains h allPassed ->
            inFunc t allPassed allDiscovered path next
        | h::t ->
            let nextCombinations = getNext h |> Seq.except allDiscovered |> Seq.toList

            inFunc t (h::allPassed) (nextCombinations @ allDiscovered) path (fun allPassed allDiscovered res ->
                match res with
                | None -> 
                    inFunc nextCombinations (allPassed) allDiscovered (h::path) next 
                | x -> x |> next allPassed allDiscovered)

    inFunc [startState] [] [startState] [] (fun _ _ c -> c)


let findPathHeuristic (startState : Configuration) (desiredState : Configuration) g h : Path option =
    let rec inFunc (combination : Configuration) path next : Path option =
        if combination = desiredState
        then Some path
        else
            match getNext combination |> Seq.toList with
            | [] -> None
            | nextCombinations ->
                let nextCombination = nextCombinations |> List.minBy (fun a -> (h a) + (g a))

                inFunc nextCombination (nextCombination :: path) next

    inFunc startState [startState] id




let printState state =
    for (i,list) in state do
        printfn "%-2i %A" i list

let printPath =
    function Some path ->
        for state in path |> List.rev do
            printState state
            printfn ""

[<EntryPoint>]
let main argv =

    //let initialState = [
    //    0,[1;2;3;4]
    //    1,[]
    //    2,[]
    //    3,[]
    //]

    //let desiredState = [
    //    0,[]
    //    1,[]
    //    2,[]
    //    3,[1;2;3;4]
    //]

    //let initialState = [
    //       0,[1;2;3]
    //       1,[]
    //       2,[]
    //   ]

    //let desiredState = [
    //    0,[]
    //    1,[]
    //    2,[1;2;3]
    //]

    let initialState = [
           0,[1;2;3]
           1,[]
           2,[]
           3,[]
       ]

    let desiredState = [
        0,[]
        1,[]
        2,[]
        3,[1;2;3]
    ]




    //let h (state : Configuration) = 
    //    (state |> List.take (List.length state - 2) |> List.map snd |> List.concat |> List.sum) 
    //    /
    //    (state |> List.last |> snd |> List.sum |> (+) 1)

    let rnd = System.Random()

    let h (state : Configuration) = 
        state |> List.map (fun (i,list) -> list |> List.sum |> (*) i |> float) |> List.sum |> (+) (rnd.NextDouble() / 100.) |> (/) 1.
          

    let g (state : Configuration) =
        1.

    let path1 = findPathDiveFirst initialState desiredState 
    let path2 = findPathBreadthFirst initialState desiredState 
    let path3 = findPathHeuristic initialState desiredState g h

    //match path1 with Some s -> printPath s
    //printPath path2

    printfn "Hello World from F#!"
    0 // return an integer exit code
