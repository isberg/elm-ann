module Genome exposing (Genome, create, addConnection, addNode, toString, toNetwork, mutate, Mutation(..))
{-| Module for doing operations on Artificial Neural Network Genomes.

@docs Genome

@docs create, addConnection, addNode, toString, toNetwork

@docs mutate, Mutation
-}

import Random exposing (Generator)
import Network exposing (Network)

{-| Genome represents an ANN genotype
-}
type Genome = Genome (List Int) (List Int) (List Int) (List (Int, Int, Float))

{-| Representing a new mutation -}
type Mutation 
    = NoMutation 
    | AddConnection Int Int Float
    | AddNode Int Int
    | ModifyWeight Int Int Float

{-| Create Genome with specified number of inputs, outputs, a bias node and no hidden nodes or connections
-}
create : Int -> Int -> Genome
create inputs outputs =
    Genome 
        (List.range 0 inputs)
        (List.range (inputs + 1) (inputs + outputs)) 
        [] 
        []

{-| Add connection between existing nodes
-}
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

{-| Add node by replacing existing connection with a node and two connections.
-}
addNode : Int -> Int -> Genome -> Genome
addNode from to genome =
    let
        (Genome inputs outputs hidden connections) = genome
        replaced = connections 
            |> List.filter (\(f, t, _)-> f == from && t == to)
        newNode = (inputs ++ outputs ++ hidden) 
            |> List.maximum 
            |> Maybe.withDefault -1
            |> (+) 1
    in
    case replaced of
        (_, _, weight) :: [] ->
            Genome inputs outputs (hidden ++ [newNode]) (connections |> List.filter (\(f, t, _) -> f /= from || t /= to ))
                |> addConnection from newNode 1
                |> addConnection newNode to weight
            
        _ -> genome



{-| convert to string representation
-}
toString : Genome -> String
toString genome =
    let
        (Genome inputs outputs hidden connections) = genome
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
    ++ " " ++ 
    (hidden |> nodes2string)
    ++ " " ++
    (connections |> connections2string)

{-| create network from genome
-}
toNetwork : Genome -> Network
toNetwork genome =
    let 
        (Genome inputs outputs hidden connections) = genome
        nodes = inputs ++ outputs ++ hidden
            |> List.map (\id -> (id, if id == 0 then 1 else 0))
    in
    Network.create 
        nodes 
        connections  

{-| mutate create a mutation generator
-}
mutate : Genome -> Generator Mutation
mutate genome =
    let
        (Genome inputs outputs hidden connections) = genome
        addConnectionGenerator =
            case (inputs ++ hidden, outputs ++ hidden) of
                (fr::om, t::o) ->
                    Random.map3
                        AddConnection
                        (Random.uniform fr om)
                        (Random.uniform t o)
                        (Random.float -2 2)
                    |> Just
                _ -> Nothing
        addNodeGenerator =
            case connections of
                con::ections
                    ->  Random.map 
                            (\(f, t, _) -> AddNode f t) 
                            (Random.uniform con ections)
                        |> Just
                _ -> Nothing
        modifyWeightGenerator = 
            case connections of
                con::ections
                    ->  Random.map2 
                            (\(f, t, _) w -> ModifyWeight f t w) 
                            (Random.uniform con ections)
                            (Random.float -0.2 0.2)
                        |> Just
                _ -> Nothing

    in
    if connections == [] then
        addConnectionGenerator |> Maybe.withDefault (Random.constant NoMutation)
    else
        Random.float 0 1
        |> Random.andThen 
            (\v -> 
                if v < 0.2 then 
                    addNodeGenerator|> Maybe.withDefault (Random.constant NoMutation)
                else if v < 0.8 then
                    modifyWeightGenerator|> Maybe.withDefault (Random.constant NoMutation)
                else
                    addConnectionGenerator |> Maybe.withDefault (Random.constant NoMutation)
            )