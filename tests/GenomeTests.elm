module GenomeTests exposing (suite)

import Expect exposing (FloatingPointTolerance(..))
import Test exposing (Test, describe, test)

import Genome
import Network


suite : Test
suite =
    describe "the Genome module"
        [ describe "Genome.create"
            [ test "create with 2 inputs, 1 output and bias" <|
                \_ ->
                    Genome.create 2 1
                        |> Genome.toString
                        |> Expect.equal "Genome [0, 1, 2] [3] [] []"
            , test "create with 0 input, 2 output and bias" <|
                \_ ->
                    Genome.create 0 2
                        |> Genome.toString
                        |> Expect.equal "Genome [0] [1, 2] [] []"
            ]
        , describe "addConnection"
            [ test "to minimal genome" <|
                \_ ->
                    Genome.create 0 1
                        |> Genome.addConnection 0 1 0.5
                        |> Genome.toString
                        |> Expect.equal "Genome [0] [1] [] [(0, 1)=0.5]"
            ]
        , describe "Genome.toNetwork"
            [ test "minimal genome creates minimal network" <|
                \_ ->
                    let
                        expected = Network.create [(0, 1)] [] 
                            |> Network.toString
                        actual = Genome.create 0 0
                            |> Genome.toNetwork
                            |> Network.toString
                    in
                    actual           
                        |> Expect.equal expected
            , test "genome with one connection" <|
                \_ ->
                    let
                        expected = Network.create [(0, 1), (1, 0)] [(0, 1, -0.5)]
                            |> Network.toString
                        actual = Genome.create 0 1 |> Genome.addConnection 0 1 -0.5
                            |> Genome.toNetwork
                            |> Network.toString
                    in
                    actual |> Expect.equal expected
            ]
        ]