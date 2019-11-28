module Example exposing (suite)

import Expect
import Test exposing (Test, describe, test)

import Network


suite : Test
suite =
    describe "the Network module"
        [ describe "Network.create"
            [ test "creates empty network" <|
                \_ ->
                    Network.create [] [] 
                        |> Network.toString
                        |> Expect.equal "Network [] []" 
            , test "create network with nodes only" <|
                \_ ->
                    Network.create [(0, 1), (1, 0), (2, 0)] []
                        |> Network.toString
                        |> Expect.equal "Network [0=1, 1=0, 2=0] []"
            , test "create network with nodes and connections" <|
                \_ ->
                    Network.create [(0, 1), (1, 0), (2, 0)] [(0, 1, -0.5)]
                        |> Network.toString
                        |> Expect.equal "Network [0=1, 1=0, 2=0] [(0, 1)=-0.5]"
            ]
        ]