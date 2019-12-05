module Genome exposing (Genome, create, addConnection, addNode, modifyWeight, toString, toNetwork, mutate, breed, Mutation(..))
{-| Module for doing operations on Artificial Neural Network Genomes.

@docs Genome

@docs create, addConnection, addNode, modifyWeight, toString, toNetwork

@docs mutate, Mutation, breed
-}

import Random exposing (Generator)
import Network exposing (Network)
import Set
import Dict
import List.Extra
import Random.Extra

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


{-| modifyWeight adds the given modification to a specific connection
-}
modifyWeight : Int -> Int -> Float -> Genome -> Genome
modifyWeight from to modification genome =
    let
        (Genome inputs outputs hidden connections) = genome
        modifiedConnections = 
            connections |> List.map modifyConnection
        modifyConnection connection = 
            let
                (f, t, w) = connection
            in
            if (f, t) == (from, to) then 
                (f, t, w + modification)
            else
                connection
    in
    Genome inputs outputs hidden modifiedConnections

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
            let
                all = 
                    List.Extra.lift2 
                        (\from to -> (from, to))
                        (inputs ++ hidden) 
                        (outputs ++ hidden)
                    |> Set.fromList
                existing = 
                    connections
                    |> List.map (\(f, t, _) -> (f, t))
                    |> Set.fromList
                candidates = 
                    Set.diff all existing
                    |> Set.toList 
                    |> List.map (\(from, to) -> AddConnection from to)
            in
            case candidates of
                (can::didates) ->
                    Random.map2
                    (\con weight -> con weight)
                    (Random.uniform can didates) 
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
                if v < 0.025 then 
                    addNodeGenerator|> Maybe.withDefault (Random.constant NoMutation)
                else if v < 0.9 then
                    modifyWeightGenerator|> Maybe.withDefault (Random.constant NoMutation)
                else
                    addConnectionGenerator |> Maybe.withDefault (Random.constant NoMutation)
            )

{-| breeds 2 genomes creating one offspring
-}
breed : Genome -> Genome -> Generator Genome
breed alpha beta =
    let
        (Genome inputs outputs hidden alpha_connections) = alpha
        (Genome _ _ _ beta_connections) = beta
        toDict l = l |> List.map (\(f, t, w) -> ((f, t), w)) |> Dict.fromList
        alphaDict = toDict alpha_connections
        betaDict = toDict beta_connections
        aligned_alpha_connections = Dict.diff alphaDict betaDict |> Dict.toList |> List.map (\((f, t), w) -> (f, t, w))
        aligned_beta_connections = Dict.diff betaDict alphaDict |> Dict.toList |> List.map (\((f, t), w) -> (f, t, w))

        connections = 
            List.map2 Random.Extra.choice aligned_alpha_connections aligned_beta_connections 
            |> Random.Extra.combine
    in
    Random.map (Genome inputs outputs hidden) connections