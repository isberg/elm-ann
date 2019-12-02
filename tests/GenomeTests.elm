module GenomeTests exposing (suite)

import Expect exposing (FloatingPointTolerance(..))
import Test exposing (Test, describe, test)
import Random

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
        , describe "addNode"
            [ test "to minimal genome" <|
                \_ ->
                    Genome.create 0 1
                        |> Genome.addConnection 0 1 -1
                        |> Genome.addNode 0 1
                        |> Genome.toString
                        |> Expect.equal "Genome [0] [1] [2] [(0, 2)=1, (2, 1)=-1]"
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
                    actual |> Expect.equal expected
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
            , test "genome with one hidden node" <|
                \_ ->
                    let
                        expected = Network.create [(0, 1), (1, 0), (2, 0)] [(0, 2, 1), (2, 1, -0.5)]
                            |> Network.toString
                        actual = Genome.create 0 1
                            |> Genome.addConnection 0 1 -0.5
                            |> Genome.addNode 0 1
                            |> Genome.toNetwork
                            |> Network.toString    
                    in
                    actual |> Expect.equal expected
            ]
        , describe "Genome.mutate"
            [ test "minimal" <|
                \_ ->
                    let
                        genome = Genome.create 0 1
                        expected = Genome.Connection 0 1 0.37920816298657556  
                        {-
                            Random.map3
                                Genome.Connection
                                (Random.int 0 0) 
                                (Random.int 1 1) 
                                (Random.float -2 2) 
                        -}
                        mutate = genome |> Genome.mutate
                        seed = Random.initialSeed 0
                        (actual, _) =
                            case mutate of 
                                Just generator ->
                                    Random.step generator seed 
                                Nothing -> 
                                    (Genome.Connection -6 -6 -6, seed)
                    in
                    actual |> Expect.equal expected

            ]
        ]