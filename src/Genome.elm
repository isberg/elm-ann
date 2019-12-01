module Genome exposing (Genome, create, toString, toNetwork)
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

{-| convert to string representation
-}
toString : Genome -> String
toString genome =
    let
        (Genome inputs outputs _ _) = genome
        nodes2string nodes =
            "[" ++ 
            (nodes |> List.map String.fromInt |> String.join ", ")
            ++ "]"
    in
    "Genome " ++
    (inputs |> nodes2string)
    ++ " " ++
    (outputs |> nodes2string)
    ++ " [] []"

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
