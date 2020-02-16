namespace Graphs

type Id = int
type Node = Id 

type Weight = float
type SourceId = int
type SinkId = int
type Connection = SourceId * SinkId * Weight

type Adjacency = Connection list

type Atom = Node * Adjacency

type Graph = Atom list

[<RequireQualifiedAccess>]
module Graph =
    let exists graph (id:Node) = 
        graph |> List.exists (fst >> (=) id)
   
    let modify (graph:Graph) (id:Node) f : Graph =
        graph |> List.map (fun x -> if fst x = id then f x else x)
   
    let connect graph source sink weight : Graph =
        modify graph source (fun x -> fst x, (source,sink,weight)::snd x)
   
    let add graph id : Graph =
        (id,[])::graph

    let confirm graph (ids:Node list) : Graph =
        List.fold (fun graph id -> if exists graph id then graph else add graph id) graph ids

    let adjacent (graph:Graph) id : Adjacency =
        graph |> List.find (fst >> (=) id) |> snd

    type GraphBuilder() =
        member _.Yield(_:unit): Graph = []
   
        [<CustomOperationAttribute("oneWay")>]
        member _.OneWay(graph:Graph,source,sink,weight) : Graph =
            let graph = confirm graph [source;sink]
            connect graph source sink weight
   
        [<CustomOperationAttribute("twoWay")>]
        member _.TwoWay(graph,source1,source2,weight) : Graph =
            let graph = confirm graph [source1;source2]
            let graph = connect graph source1 source2 weight
            connect graph source2 source1 weight
