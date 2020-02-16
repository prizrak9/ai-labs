// Learn more about F# at http://fsharp.org

open Graphs

//let graph = Graph.GraphBuilder()


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

    printfn "graph: %A" graph


    let path = PathFinding.DepthFirstSearch.findFirstNonIntersectingPath graph 12 12
    printfn "first path: %A" path

    let path = PathFinding.DepthFirstSearch.findLongestNonIntersectingPath graph 1 1
    printfn "longest path: %A" path

    
    printfn "Hello World from F#!"
    0 // return an integer exit code
