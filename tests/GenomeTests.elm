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
        , describe "modifyWeight"
            [ test "updates weight" <|
                \_ ->
                    Genome.create 0 1
                    |> Genome.addConnection 0 1 -0.1
                    |> Genome.modifyWeight 0 1 0.2
                    |> Genome.toString
                    |> Expect.equal "Genome [0] [1] [] [(0, 1)=0.1]"
                    
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
            [ test "minimal add connection" <|
                \_ ->
                    let
                        genome = Genome.create 0 1
                        expected = Genome.AddConnection 0 1 -1.27545817358676  
                        mutate = genome |> Genome.mutate
                        seed = Random.initialSeed 0
                    in
                    case mutate of 
                        generator ->
                            let
                                (actual, _) = Random.step generator seed
                            in
                            actual |> Expect.equal expected 
            , test "minimal, one connection adds Node" <|
                \_ ->
                    let
                        genome = 
                            Genome.create 0 1
                            |> Genome.addConnection 0 1 -0.5
                        expected = Genome.AddNode 0 1
                        mutate = genome |> Genome.mutate
                        seed = Random.initialSeed 15
                    in
                    case mutate of 
                        generator ->
                            let
                                (actual, _) = Random.step generator seed
                            in
                            actual |> Expect.equal expected 
            , test "minimal, one connection updates weight" <|
                \_ ->
                    let
                        genome = 
                            Genome.create 0 1
                            |> Genome.addConnection 0 1 -0.5
                        expected = Genome.ModifyWeight 0 1 0.16613396827782934
                        mutate = genome |> Genome.mutate
                        seed = Random.initialSeed 1
                    in
                    case mutate of 
                        generator ->
                            let
                                (actual, _) = Random.step generator seed
                            in
                            actual |> Expect.equal expected 
            ]
        ]