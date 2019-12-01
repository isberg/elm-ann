module Genome exposing (Genome, create, addConnection, toString, toNetwork)
{-| Module for doing operations on Artificial Neural Network Genomes.
-}

import Network exposing (Network)

{-| Genome represents an ANN genotype
-}
type Genome = Genome (List Int) (List Int) (List Int) (List (Int, Int, Float))

{-| Create Genome with specified number of inputs, outputs, a bias node and no hidden nodes or connections
-}
create : Int -> Int -> Genome
create inputs outputs =
    Genome 
        (List.range 0 inputs)
        (List.range (inputs + 1) (inputs + outputs)) 
        [] 
        []

addConnection : Int -> Int -> Float -> Genome -> Genome
addConnection from to weight genome =
    let
        (Genome inputs outputs hidden connections) = genome
    in
    Genome 
        inputs 
        outputs 
        hidden 
        (connections ++ [(from, to, weight)])

{-| convert to string representation
-}
toString : Genome -> String
toString genome =
    let
        (Genome inputs outputs _ connections) = genome
        nodes2string nodes =
            "[" ++ 
            (nodes |> List.map String.fromInt |> String.join ", ")
            ++ "]"
        connections2string cons =
            "[" ++
            (cons |> List.map con2string |> String.join ", ")
            ++ "]"
        con2string (from, to, weight) =
            "(" ++
            (from |> String.fromInt)
            ++ ", " ++
            (to |> String.fromInt)
            ++ ")=" ++
            (weight |> String.fromFloat)
    in
    "Genome " ++
    (inputs |> nodes2string)
    ++ " " ++
    (outputs |> nodes2string)
    ++ " [] " ++
    (connections |> connections2string)

toNetwork : Genome -> Network
toNetwork genome =
    let 
        (Genome inputs outputs hidden connections) = genome
        nodes = inputs ++ outputs 
            |> List.map (\id -> (id, if id == 0 then 1 else 0))
    in
    Network.create 
        nodes 
        connections  
