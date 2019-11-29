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
        , describe "Network.activate"
            [ test "activate without connections changes nothing" <|
                \_ ->
                    Network.create [(0, 1), (1, 0), (2, -0.5)] []
                        |> Network.activate
                        |> Network.toString
                        |> Expect.equal "Network [0=1, 1=0, 2=-0.5] []"
            , test "activate with connections calculates output" <|
                \_ ->
                    Network.create [(0, 1), (1, 0)] [(0, 1, 0.5)]
                        |> Network.activate
                        |> Network.toString
                        |> Expect.equal "Network [0=1, 1=1] [(0, 1)=0.5]"
            ]
        , describe "Network.setValues"
            [ test "sets all matching node value" <|
                \_ ->
                    let
                        dc = 666
                    in
                    Network.create [(0, dc), (1, dc), (2, dc)] []
                        |> Network.setValues [(0, 1), (1, -0.5)]
                        |> Network.toString
                        |> Expect.equal "Network [0=1, 1=-0.5, 2=666] []"
            ]
        , describe "Network.toDot"
            [ test "describes empty graph" <|
                \_ ->
                    Network.create [] []
                        |> Network.toDot
                        |> Expect.equal "digraph {\n\n\n}"
            , test "describes graph with nodes only" <|
                \_ ->
                    Network.create [(0, 1), (1, 0)] []
                        |> Network.toDot
                        |> Expect.equal """digraph {\n0 [label="0=1"]\n1 [label="1=0"]\n\n}"""
            , test "describes graph with nodes and edges" <|
                \_ ->
                    Network.create [(1, 0)] [(1, 1, -0.5)]
                        |> Network.toDot
                        |> Expect.equal """digraph {\n1 [label="1=0"]\n1 -- 1 [label="-0.5"]\n}"""
            ]
        ]