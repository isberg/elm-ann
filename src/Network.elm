module Network exposing (Network, create, toString)

{-| Basic module for creating and using Artificial Neural Networks (ANNs).

@docs Network

@docs create, toString
-}

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