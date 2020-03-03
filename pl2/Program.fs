// Learn more about F# at http://fsharp.org

open System

type Pole<'a> = Pole of int * list<'a>

module List =
    let skip n list =
        if n > List.length list
        then []
        else List.skip n list

let printState state =
    for (i,list) in state do
        printfn "%-2i %A" i list

let printPath =
    function Some path ->
        for state in path |> List.rev do
            printState state
            printfn ""


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


type State = (int*int list) list
type Path = State list

// Head recursion
let findPathDiveFirstHead (poles : State) (desiredState : State) : Path option =
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
let findPathDiveFirst (startState : State) (desiredState : State) : Path option =
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

type Configuration = State * Path
type Queue = Configuration list

let findPathBreadthFirst (startState : State) (desiredState : State) : Path option =
    let rec inFunc (queue : Queue) (allPassed : State list) : Path option =
        match queue with
        | [] -> 
            None
        | (state, path)::_ when state = desiredState ->
            state::path |> Some
        | (state, path)::t ->
            let nextCombinations : Queue = 
                state
                |> getNext
                |> Seq.except allPassed 
                |> Seq.map (fun a -> a, state::path)
                |> Seq.toList
                
            inFunc (t@nextCombinations) (state::allPassed)

    inFunc [startState,[]] []




let findPathHeuristic (startState : State) (desiredState : State) g h : Path option =
    let rec inFunc (combination : State) path next : Path option =
        if combination = desiredState
        then Some path
        else
            match getNext combination |> Seq.toList with
            | [] -> None
            | nextCombinations ->
                let nextCombination = nextCombinations |> List.minBy (fun a -> (h a) + (g a))

                inFunc nextCombination (nextCombination :: path) next

    inFunc startState [startState] id





[<EntryPoint>]
let main argv =

    let initialState = [
        0,[1;2;3;4]
        1,[]
        2,[]
        3,[]
    ]

    let desiredState = [
        0,[]
        1,[]
        2,[]
        3,[1;2;3;4]
    ]

    //let initialState = [
    //       0,[1;2;3]
    //       1,[]
    //       2,[]
    //   ]

    //let desiredStateStateState   //   State    2,[1;2;3]
    //]

    //let initialState = [
    //       0,[1;2;3]
    //       1,[]
    //       2,[]
    //       3,[]
    //   ]

    //let desiredState = [
    //    0,[]
    //    1,[]
    //    2,[]
    //    3,[1;2;3]
    //]


    let rnd = System.Random()

    let h (state : State) = 
        state |> List.map (fun (i,list) -> list |> List.sum |> (*) i |> float) |> List.sum  |> (/) 1. |> (+) (rnd.NextDouble() / 100.)
          

    let g (state : State) =
        1.

    let path1 = findPathDiveFirst initialState desiredState 
    let path2 = findPathBreadthFirst initialState desiredState 
    let path3 = findPathHeuristic initialState desiredState g h

    //match path1 with Some s -> printPath s
    //printPath path2

    printfn "Hello World from F#!"
    0 // return an integer exit code
