module PathFinding
open Graphs

type Length = float
type Path = SinkId list * Length

module DepthFirstSearch =
    // Head recursion
    let findFirstPath graph (start:SinkId) (finish:SinkId) : Path option =
        let rec inFunc (connections:Adjacency) : Path option =
            match connections with
            | (_,_,sink,weight)::t ->
                if sink = finish
                then Some ([sink], weight) 
                else
                    match inFunc (Graph.adjacent graph sink) with
                    | Some (path,length) -> Some (sink::path, length + weight) 
                    | None -> inFunc t

            | _ -> None

        inFunc (Graph.adjacent graph start)


    module private List =
        // Conditional fold.
        let rec foldCond (func:'State->'T->'State) (preCond:'State->bool) (postCond:'State->bool) (state:'State) (collection:'T list) : 'State =
            match collection with
            | [] -> state
            | h::t ->
                if preCond state
                then
                    let state = func state h
                    if postCond state
                    then foldCond func preCond postCond state t
                    else state
                else state

    type private Command<'a> =
        | Continue of 'a
        | Finish of 'a
  
    type private DetailedPath = (SinkId * ConnectionId) list * Length

    let findFirstNonIntersectingPath graph (start:SinkId) (finish:SinkId) : Path option =
        let succeed _ = true
        let cont = function Continue _ -> true | _ -> false

        let rec inFunc (Continue (lastRoute,lastLength) as lastCommand:Command<DetailedPath>) ((id,_,sink,weight):Connection) : Command<DetailedPath>  =
            if List.contains sink (List.map fst lastRoute)
            // The snake bit itself. Return
            then lastCommand
            else
                let currentPath = (sink,id)::lastRoute, lastLength + weight
                if sink = finish
                then
                    // This check is only required for cyclic graphs.
                    // If the start and finish are the same node
                    // then we need to prevent using the same connection
                    // to return right after visiting the first node
                    if List.length lastRoute = 1 && (List.head lastRoute |> snd) = id
                    then lastCommand
                    else Finish currentPath
                // Dive
                else
                    let state = (Continue currentPath)
                    match List.foldCond inFunc succeed cont state (Graph.adjacent graph sink) with
                    | Continue _ -> lastCommand
                    | a -> a

        match List.foldCond inFunc succeed cont (Continue ([],0.)) (Graph.adjacent graph start) with
        | Continue _ -> None
        | Finish (route, length)-> Some (route |> List.map fst |> List.rev, length)

    type private LongestPath = DetailedPath
    type private CurrentPath = DetailedPath
    type private State = LongestPath * CurrentPath

    // Mixed recursion
    // Fold is tail-recursed but inFunc is head-recursed
    let findLongestNonIntersectingPath graph (start:SinkId) (finish:SinkId) : DetailedPath option =
        let rec inFunc (state:State) ((id,_,sink,weight):Connection) : State =
            let ((_,longestLength) as longestPath), ((lastRoute,lastLength) as lastPath) = state

            // This check is only required for cyclic graphs
            if List.contains sink (List.map fst lastRoute)
            // The snake bit itself. Return
            then longestPath, lastPath
            else
                let (_,currentLength) as currentPath = (sink,id)::lastRoute, lastLength + weight

                if sink = finish
                then 
                    // This check is only required for cyclic graphs.
                    // If the start and finish are the same node
                    // then we need to prevent using the same connection
                    // to return right after visiting the first node
                    if List.length lastRoute = 1 && (List.head lastRoute |> snd) = id
                    then longestPath, lastPath
                    else
                        if currentLength > longestLength
                        // We found new longestPath. Update longestPath and return
                        then currentPath, lastPath
                        // Return
                        else longestPath, lastPath
                // Dive
                else
                    let state = longestPath, currentPath
                    let (longestPath,_) = List.fold inFunc state (Graph.adjacent graph sink)
                    longestPath, lastPath

        match List.fold inFunc (([],0.),([],0.)) (Graph.adjacent graph start) with
        | ([],_),_ -> None
        | (longestRoute,longestLength),_ -> Some (longestRoute |> List.rev, longestLength)
