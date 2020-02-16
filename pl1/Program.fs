// Learn more about F# at http://fsharp.org

open Graphs

let printgraph (graph:Graph) =
    let out = 
        let printConn = 
            function 
            | (Some id,_,sinkId,weight) -> sprintf "%i|%i:%g" sinkId id weight 
            | (_,_,sinkId,weight) -> sprintf "%i:%g" sinkId weight

        let printNode (node, adj) = sprintf "%i: %s" node (adj |> List.map printConn |> String.concat "; ")

        graph 
        |> List.map printNode
        |> String.concat "\n"

    printfn "graph:\n%s\n" out
  
let printPath a b path =
    match path with
    | Some path ->
        printfn "first path from %i to %i: %A" a b path
    | _ ->
        printfn "first path from %i to %i: not found" a b

let printDetailedPath a b path =
    match path with
    | Some path ->
        let formatForOutput = path |> fst |> List.map (function (sinkId,Some connId) -> sprintf "%i:%i" sinkId connId | (sinkId,_) -> sprintf "%i" sinkId) |> String.concat "; "
        printfn "longest path from %i to %i: ([%s], %g)" a b formatForOutput (snd path)
    | _ ->
        printfn "longest path from %i to %i: not found" a b

[<EntryPoint>]
let main argv =
    let graph = Graph.GraphBuilder(defaultWeight = 1., allConnectionsUnique = true) {
        t 1 2
        t 1 10
        t 1 11
        t 2 3
        t 2 4
        t 3 5
        t 3 6
        t 4 5
        t 4 9
        t 5 6
        t 5 13 
        t 5 18 
        t 6 18 
        t 6 11 
        t 7 8 
        t 7 12 
        t 7 2 
        t 8 10 
        t 9 14 
        t 11 16 
        t 13 1 
        t 14 17 
        t 15 16 
        t 16 17

        // Demonstrate use of one way unique connections
        tux 1 7
    }

    printgraph graph

    let a = 1
    let b = 1

    let path = PathFinding.DepthFirstSearch.findFirstNonIntersectingPath graph a b
    printPath a b path

    let path = PathFinding.DepthFirstSearch.findLongestNonIntersectingPath graph a b
    printDetailedPath a b path

    0 // return an integer exit code
