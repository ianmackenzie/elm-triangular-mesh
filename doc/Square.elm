module Square exposing (..)

import Array.Hamt as Array exposing (Array)
import Html exposing (Html)
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.Mesh as Mesh exposing (Mesh)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Svg as Svg
import Svg
import Svg.Attributes as Attributes


size : Float
size =
    100


main : Html Never
main =
    let
        a =
            Point2d.fromCoordinates ( 0, 0 )

        b =
            Point2d.fromCoordinates ( size, 0 )

        c =
            Point2d.fromCoordinates ( size, size )

        d =
            Point2d.fromCoordinates ( 0, size )

        vertices =
            [ a, b, c, d ]

        square =
            Mesh.with
                { vertices = Array.fromList vertices
                , faceIndices = [ ( 0, 1, 2 ), ( 0, 2, 3 ) ]
                }

        offset =
            30

        bounds =
            BoundingBox2d.with
                { minX = -offset
                , maxX = size + offset
                , minY = -offset
                , maxY = size + offset
                }

        edges =
            Mesh.edges square |> List.map LineSegment2d.fromEndpoints
    in
    Svg.render2d bounds <|
        Svg.g []
            [ Svg.g [ Attributes.stroke "black", Attributes.strokeWidth "0.5" ]
                (List.map (Svg.lineSegment2d []) edges)
            , Svg.g [] (List.map (Svg.point2dWith { radius = 2 } []) vertices)
            , Svg.g [ Attributes.fontSize "18" ]
                [ Svg.text2d
                    [ Attributes.textAnchor "end"
                    , Attributes.dominantBaseline "hanging"
                    ]
                    a
                    "a"
                , Svg.text2d
                    [ Attributes.textAnchor "start"
                    , Attributes.dominantBaseline "hanging"
                    ]
                    b
                    "b"
                , Svg.text2d
                    [ Attributes.textAnchor "start"
                    , Attributes.dominantBaseline "baseline"
                    ]
                    c
                    "c"
                , Svg.text2d
                    [ Attributes.textAnchor "end"
                    , Attributes.dominantBaseline "baseline"
                    ]
                    d
                    "d"
                ]
            ]
