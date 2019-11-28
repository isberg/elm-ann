module Network exposing (Network, create)

{-| Basic module for creating and using Artificial Neural Networks (ANNs).

@docs Network

@docs create
-}

{-| Network
    Network.create [1, 2] [(1, 2, -0.5)]
-}
type Network = Network (List Int) (List (Int, Int, Float))

{-| create function
-}
create : (List Int) -> (List (Int, Int, Float)) -> Network
create nodes connections =
    Network nodes connections
