namespace Graphs

type Id = int

type NodeId = Id
type Node = NodeId 

type Weight = float
type SourceId = NodeId
type SinkId = NodeId
type ConnectionId = Id option
type Connection = ConnectionId * SourceId * SinkId * Weight

type Adjacency = Connection list

type Atom = Node * Adjacency

type Graph = Atom list

[<RequireQualifiedAccess>]
module Graph =
    let exists graph (id:Node) = 
        graph |> List.exists (fst >> (=) id)
   
    let modify (graph:Graph) (id:Node) f : Graph =
        graph |> List.map (fun x -> if fst x = id then f x else x)
   
    let add graph id : Graph =
        (id,[])::graph

    let confirm graph (ids:Node list) : Graph =
        List.fold (fun graph id -> if exists graph id then graph else add graph id) graph ids

    let adjacent (graph:Graph) id : Adjacency =
        graph |> List.find (fst >> (=) id) |> snd

    let maxId (graph:Graph) =
        graph 
        |> List.map (snd >> List.map (fun (id,_,_,_) -> id)) 
        |> List.concat
        |> List.choose id
        |> function 
        | [] -> None
        | x -> 
            x 
            |> List.max 
            |> Some

    let connByNodes (graph:Graph) a b : Connection list =
        graph
        |> List.filter (fun (node, _) -> node = a || node = b)
        |> List.map (snd)
        |> List.concat
        |> List.filter (fun (_,c,d,_) -> c = a && d = b) 

    let connectionExists graph (id:Id option) source sink _ =
        connByNodes graph source sink
        |> List.exists (fun (i,_,_,_) -> i = id)

    let connect graph (id:Id option) source sink weight : Graph =
        modify graph source (fun x -> fst x, (id,source,sink,weight)::snd x)

    let connectUnique graph (id:Id option) source sink weight : Graph =
        if connectionExists graph id source sink weight
        then failwith "connection already exists"
        else modify graph source (fun x -> fst x, (id,source,sink,weight)::snd x)

    // Does not guarantee order of adjacencies
    type GraphBuilder(?defaultWeight, ?allConnectionsUnique) =
        let defaultWeight =
            match defaultWeight with
            | None -> 0.
            | Some s -> s

        let connect = 
            match allConnectionsUnique with
            | Some true -> connectUnique
            | _ -> connect
        
        let getFreeId graph =
            match maxId graph with 
            | Some x -> x + 1
            | _ -> 0

        member _.Yield(_:unit): Graph = []
        
        [<CustomOperationAttribute("oiw")>]
        member _.OneWayIdentifiedWeighted(graph:Graph,id,source,sink,weight) : Graph =
            let graph = confirm graph [source;sink]
            connect graph (Some id) source sink weight

        [<CustomOperationAttribute("oi")>]
        member _.OneWayIdentified(graph:Graph,id,source,sink) : Graph =
            let graph = confirm graph [source;sink]
            connect graph (Some id) source sink defaultWeight

        [<CustomOperationAttribute("o")>]
        member _.OneWay(graph:Graph,source,sink) : Graph =
            let graph = confirm graph [source;sink]
            connect graph None source sink defaultWeight

        [<CustomOperationAttribute("ou")>]
        member _.OneWayUnique(graph:Graph,source,sink) : Graph =
            let id = getFreeId graph |> Some
            let graph = confirm graph [source;sink]
            connect graph id source sink defaultWeight

        [<CustomOperationAttribute("t")>]
        member _.TwoWay(graph,source1,source2) : Graph =
            let graph = confirm graph [source1;source2]
            let graph = connect graph None source1 source2 defaultWeight
            connect graph None source2 source1 defaultWeight

        [<CustomOperationAttribute("tu")>]
        member _.TwoWayUnique(graph,source1,source2) : Graph =
            let id = getFreeId graph |> Some
            let graph = confirm graph [source1;source2]
            let graph = connect graph id source1 source2 defaultWeight
            connect graph id source2 source1 defaultWeight

        [<CustomOperationAttribute("tux")>]
        member _.TwoWayUniqueExclusively(graph,source1,source2) : Graph =
            let id = getFreeId graph
            let graph = confirm graph [source1;source2]
            let graph = connect graph (Some id) source1 source2 defaultWeight
            connect graph (Some (id + 1)) source2 source1 defaultWeight

