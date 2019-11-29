module Network exposing (Network, create, activate, setValues, toString, toDot)

{-| Basic module for creating and using Artificial Neural Networks (ANNs).

@docs Network

@docs create, setValues, activate, toString, toDot
-}

import Dict

{-| Representing an Artificial Neural Network
    
    Network.create [(1, 0.5), (2, -0.1)] [(1, 2, -0.5)]
-}
type Network = Network (List (Int, Float)) (List (Int, Int, Float))

{-| create function

    Network.create [(1, 0.5), (2, -0.1)] [(1, 2, -0.5)]
-}
create : (List (Int, Float)) -> (List (Int, Int, Float)) -> Network
create nodes connections =
    Network nodes connections

{-| setValues is used to set values of input nodes

    someNetwork |> Network.setValues [(0, 1), (1, -0.5)]
-}
setValues : List (Int, Float) -> Network -> Network
setValues newValues network =
    let
        (Network oldValues connections) = network
        oldies =  Dict.fromList oldValues
        newNodes = oldies 
            |> Dict.union (Dict.intersect (Dict.fromList newValues) oldies)
            |> Dict.toList
    in
    Network newNodes connections

{-| activate updates the network node values using the Step function

    someNetwork |> Network.activate 
-}
activate : Network -> Network
activate network =
    let
        (Network nodes connections) = network
        step z = if z > 0 then 1 else 0
        activateNode (id, value) = 
            let 
                getOutput nodeId = nodes 
                    |> List.filter (\(nid, _) -> nodeId == nid )
                    |> List.map (\(_, val) -> val)
                    |> List.head 
                    |> Maybe.withDefault 0
                inputs = connections 
                    |> List.filter (\(_, to, _) -> id == to)
                insignal = inputs 
                    |> List.map (\(from, _, weight) -> weight * getOutput from)
                    |> List.sum
                newValue =
                    case inputs of
                       [] -> value
                       _ -> insignal |> step
            in
            (id, newValue)
    in
    Network (nodes |> List.map activateNode) connections
    

{-| convert Network to String representation 
    
    someNetwork |> Network.toString
-}
toString : Network -> String
toString network =
    let
        (Network nodes connections) = network
        node2string (id, value) = 
            (id |> String.fromInt) ++ "=" ++ (value |> String.fromFloat) 
        connection2string (from, to, weight) = 
            "(" ++
            (from |> String.fromInt) 
            ++ ", " ++ 
            (to |> String.fromInt) 
            ++ ")=" ++ 
            (weight |> String.fromFloat) 
    in
    "Network [" ++ 
    (nodes |> List.map node2string |> String.join ", ")
    ++ "] [" ++
    (connections |> List.map connection2string |> String.join ", ")
    ++ "]"

{-| toDot create graph description
see https://en.wikipedia.org/wiki/DOT_(graph_description_language)
can be vizualised with https://rise4fun.com/agl
or https://dreampuf.github.io/GraphvizOnline/
or using ports with https://github.com/mdaines/viz.js/

    Network.create [(0, 1), (1, 0)] [(0, 1, -0.5)]
        |> Network.toDot
        -- == 
        digraph {
            0 [label="0=1"]
            1 [label="1=0"]
            0 -- 1 [label="-0.5"]
        }
-}
toDot : Network -> String
toDot network = 
    let
        (Network nodes connections) = network
        nodesAsDot = nodes |> List.map node2dot |> String.join "\n"
        node2dot (id, value) =
            let
                sid = id |> String.fromInt
                svalue = value |> String.fromFloat
            in
            sid ++ " [label=\"" ++ sid ++ "=" ++ svalue  ++ "\"]"  

        connectionsAsDot = connections |> List.map connection2dot |> String.join "\n"
        connection2dot (from, to, weight) = 
            (from |> String.fromInt)
            ++ " -> " ++
            (to |> String.fromInt)
            ++ " [label=\"" ++ (weight |> String.fromFloat) ++ "\"]"
    in
    "digraph {\n" ++ 
    nodesAsDot
    ++ "\n" ++
    connectionsAsDot 
    ++"\n}"