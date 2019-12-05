module Network exposing (Network, create, activate, setValues, toString, toDot, fitness, get)

{-| Basic module for creating and using Artificial Neural Networks (ANNs).

@docs Network

@docs create, setValues, activate, toString, toDot, fitness, get
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
        sigmoid z = 1 / (1 + e^ -z)
        activationFunction = sigmoid
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
                       _ -> insignal |> activationFunction
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

{-| fitness function defined as
1 - RMSE = 1 - Root of Mean Squared Error, this calcuation is done per sample and then summed for all samples,
which should mean the result gives how many correct answer the network gave (kind of).
see https://www.researchgate.net/figure/The-root-mean-squared-error-RMSE-when-the-genetic-algorithm-is-implemented-with-fitness_fig1_319382166

    Network.create [(1, 0), (2, 0)] [(1, 2, 1.0)]
                        |> Network.fitness 
                            [ ([(1, 0)], [(2, 0)])
                            , ([(1, 1)], [(2, 1)])
                            ]
    -- == 2.0  (Meaning both samples correctly answered)

-}
fitness : List (List (Int, Float), List (Int, Float)) -> Network -> Float
fitness data network =
    let
        evaluateOne : List (Int, Float) -> List Int -> List (Int, Float)
        evaluateOne input outputIds =
            let
                actual = network 
                    |> setValues input 
                    |> activate
                    |> activate
                    |> activate
                    |> get outputIds
            in
            actual
        error expected actual =
            let
                sumOfErrorSquared = 
                    List.map2 (\ex ac -> (ex-ac) ^ 2) expected actual
                    |> List.sum 
                len =  List.length expected |> toFloat
                avg = sumOfErrorSquared / len
            in
            1 - sqrt avg
        idsOnly : (List (Int, Float)) -> List Int
        idsOnly nodes = nodes |> List.map (\(id, _) -> id)
        valuesOnly nodes = nodes |> List.map (\(_, value) -> value)
        errorPerSample : (List (Int, Float), List (Int, Float)) -> Float 
        errorPerSample (input, expected) =
            let
                actual = evaluateOne input (expected |> idsOnly)
                err = error (expected |> valuesOnly) (actual |> valuesOnly)
            in
            err
    in
    List.map errorPerSample data |> List.sum

{-| get specific nodes with their current values
-}
get : List Int -> Network -> List (Int, Float)
get nodeIds network =
    let
        (Network nodes _) = network
    in
    Dict.intersect 
        (nodes |> Dict.fromList) 
        (nodeIds |> List.map (\id -> (id, 0)) |> Dict.fromList)
        |> Dict.toList