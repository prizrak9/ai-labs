// Learn more about F# at http://fsharp.org

open Graphs

let graph = Graph.GraphBuilder()


[<EntryPoint>]
let main argv =
    //// Legacy way
    //let graph : Graph =
    //    [
    //        1, [1,2,0.5]
    //        2, [2,1,0.5]
    //    ]
    
    //// Builder way
    //let graph = GraphBuilder(){
    //    twoWay 1 2 0.5
    //}


    let o = 1.
    let graph = graph {
        twoWay 1 2 o
        twoWay 1 7 o
        twoWay 1 10 o
        twoWay 1 11 o
        twoWay 2 3 o
        twoWay 2 4 o
        twoWay 3 5 o
        twoWay 3 6 o
        twoWay 4 5 o
        twoWay 4 9 o
        twoWay 5 6 o
        twoWay 5 13 o
        twoWay 5 18 o
        twoWay 6 18 o
        twoWay 6 11 o
        twoWay 7 8 o
        twoWay 7 1 o
        twoWay 7 12 o
        twoWay 7 2 o
        twoWay 8 10 o
        twoWay 9 14 o
        twoWay 10 20 o
        twoWay 11 16 o
        twoWay 13 1 o
        twoWay 14 17 o
        twoWay 15 16 o
        twoWay 16 17 o
    }

    let path = PathFinding.DepthFirstSearch.findFirstNonIntersectingPath graph 1 1
    //let path = PathFinding.DepthFirstSearch.findLongestNonIntersectingPath graph 1 1
    
    printfn "%A" path

    
    printfn "Hello World from F#!"
    0 // return an integer exit code
