module GenomeTests exposing (suite)

import Expect exposing (FloatingPointTolerance(..))
import Test exposing (Test, describe, test)

import Genome


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
        ]