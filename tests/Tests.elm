module Tests exposing (..)

import Array.Hamt as Array
import Expect exposing (Expectation)
import Test exposing (Test)
import TriangularMesh exposing (TriangularMesh)


square : TriangularMesh Char
square =
    let
        vertices =
            Array.fromList [ 'a', 'b', 'c', 'd' ]

        faceIndices =
            [ ( 0, 1, 2 ), ( 0, 2, 3 ) ]
    in
    TriangularMesh.indexed vertices faceIndices


triangle : TriangularMesh Char
triangle =
    let
        vertices =
            Array.fromList [ 'e', 'f', 'g' ]

        faceIndices =
            [ ( 0, 1, 2 ) ]
    in
    TriangularMesh.indexed vertices faceIndices


faceVertices : Test
faceVertices =
    Test.test "faceVertices"
        (\() ->
            TriangularMesh.faceVertices square
                |> Expect.equal
                    [ ( 'a', 'b', 'c' )
                    , ( 'a', 'c', 'd' )
                    ]
        )


edgeVertices : Test
edgeVertices =
    Test.test "edgeVertices"
        (\() ->
            TriangularMesh.edgeVertices square
                |> Expect.equal
                    [ ( 'a', 'b' )
                    , ( 'a', 'c' )
                    , ( 'a', 'd' )
                    , ( 'b', 'c' )
                    , ( 'c', 'd' )
                    ]
        )


combine : Test
combine =
    Test.test "combine"
        (\() ->
            TriangularMesh.combine [ triangle, square ]
                |> Expect.all
                    [ TriangularMesh.vertices
                        >> Array.toList
                        >> Expect.equal [ 'e', 'f', 'g', 'a', 'b', 'c', 'd' ]
                    , TriangularMesh.faceIndices
                        >> Expect.equal
                            [ ( 0, 1, 2 )
                            , ( 3, 4, 5 )
                            , ( 3, 5, 6 )
                            ]
                    , TriangularMesh.faceVertices
                        >> Expect.equal
                            [ ( 'e', 'f', 'g' )
                            , ( 'a', 'b', 'c' )
                            , ( 'a', 'c', 'd' )
                            ]
                    ]
        )


fan : Test
fan =
    Test.describe "fan"
        [ Test.test "Three fan vertices"
            (\() ->
                TriangularMesh.fan 'a' [ 'b', 'c', 'd' ]
                    |> TriangularMesh.faceVertices
                    |> Expect.equal [ ( 'a', 'b', 'c' ), ( 'a', 'c', 'd' ) ]
            )
        , Test.test "Two fan vertices"
            (\() ->
                TriangularMesh.fan 'a' [ 'b', 'c' ]
                    |> TriangularMesh.faceVertices
                    |> Expect.equal [ ( 'a', 'b', 'c' ) ]
            )
        , Test.test "One fan vertex"
            (\() ->
                TriangularMesh.fan 'a' [ 'b' ]
                    |> Expect.equal TriangularMesh.empty
            )
        , Test.test "No fan vertices"
            (\() ->
                TriangularMesh.fan 'a' []
                    |> Expect.equal TriangularMesh.empty
            )
        ]
