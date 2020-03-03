// Learn more about F# at http://fsharp.org

open System

type Pole<'a> = int * list<'a>

type Pole = Pole<int>
type State = Pole list
type Path = State list
type Configuration = State * Path
type Queue = Configuration list


let getNext (poles:State) = seq {
    let get i j list t =
        if i < j
        then (List.take i poles) @ [i,t] @ (poles |> List.skip (i+1) |> List.take (j-i-1)) @ [j,list] @ (List.skip (j+1) poles)
        else (List.take j poles) @ [j,list] @ (poles |> List.skip (j+1) |> List.take (i-j-1)) @ [i,t] @ (List.skip (i+1) poles)

    for source in poles do
        for sink in poles |> Seq.except [source] do
            match source, sink with
            | (i,h1::t1), (j,h2::t2) when h2 > h1-> 
                yield get i j (h1::h2::t2) t1
            | (i,h1::t1), (j,[]) ->
                yield get i j [h1] t1
            | _ -> ()
}


let findPathDiveFirstHead (poles : State) (desiredState : State) : Path option =
    let rec inFunc left path : Path option =
        match left with
        | [] -> 
            None
        | h::_ when h = desiredState ->
           h::path |> Some
        | h::t ->
            let nextCombinations = getNext h |> Seq.except path |> Seq.toList
            match inFunc nextCombinations (h::path) with
            | None -> inFunc t path
            | x -> x

    inFunc [poles] []

let findPathDiveFirstTail (startState : State) (desiredState : State) : Path option =
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
                |> List.ofSeq
                |> List.except allPassed 
                |> List.map (fun a -> a, state::path)
                
            inFunc (nextCombinations@t) (state::allPassed)

    inFunc [startState,[]] []

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
                |> List.ofSeq
                |> List.except allPassed 
                |> List.map (fun a -> a, state::path)
                
            inFunc (t@nextCombinations) (state::allPassed)

    inFunc [startState,[]] []

let findPathHeuristic (startState : State) (desiredState : State) : Path option =
    let rec inFunc (queue : Queue) (allPassed : State list) : Path option =
        match queue with
        | [] -> 
            None
        | (state, path)::_ when state = desiredState ->
            state::path |> Some
        | (state, path)::t ->
            let g state =
                path |> Seq.filter ((=) state) |> Seq.length |> float 

            let h (state : State) = 
                state |> List.map (fun (i,list) -> list |> List.sum |> (*) i |> float) |> List.sum  |> (/) 1.

            let nextCombinations : Queue = 
                state
                |> getNext
                |> List.ofSeq
                |> List.except allPassed 
                |> List.map (fun a -> a, state::path)
                |> (@) t
                |> List.sortBy (fun (a,_) -> (h a) + (g a))
                
            inFunc nextCombinations (state::allPassed)

    inFunc [startState,[]] []




let printPath =
    function Some path ->
        for state in path |> List.rev do
            for (i,list) in state do
                printfn "%-2i %A" i list
            printfn ""

let printLength name =
    function Some path ->
        printfn "%s path length: %i" name (List.length path)

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
    //    0,[1;2;3;4;5]
    //    1,[]
    //    2,[]
    //]

    //let desiredState = [
    //    0,[]
    //    1,[]
    //    2,[1;2;3;4;5]
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


    //let path1 = findPathDiveFirstHead initialState desiredState 
    let path2 = findPathDiveFirstTail initialState desiredState 
    let path3 = findPathBreadthFirst initialState desiredState 
    let path4 = findPathHeuristic initialState desiredState

    printLength "findPathDiveFirstTail" path2
    printLength "findPathBreadthFirst" path3
    printLength "findPathHeuristic" path4

    //printPath path4

    0 // return an integer exit code
