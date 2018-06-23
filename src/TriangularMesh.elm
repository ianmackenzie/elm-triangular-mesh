module TriangularMesh
    exposing
        ( TriangularMesh
        , combine
        , edgeIndices
        , edgeVertices
        , empty
        , faceIndices
        , faceVertices
        , fan
        , indexed
        , mapVertices
        , strip
        , triangles
        , vertex
        , vertices
        )

{-| This module provides functions for working with indexed triangular meshes.
You can:

  - Construct meshes from vertices and face indices
  - Extract vertices, faces and edges in various ways
  - Combine multiple meshes into a single mesh

@docs TriangularMesh


# Constants

@docs empty


# Constructors

@docs indexed, triangles, fan, strip, combine


# Properties

@docs vertices, vertex, faceIndices, faceVertices, edgeIndices, edgeVertices


# Transformations

@docs mapVertices

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)


{-| A `TriangularMesh` is a simple form of [face-vertex mesh](https://en.wikipedia.org/wiki/Polygon_mesh#Face-vertex_meshes)
defined by an array of vertices and a list of face indices. Each set of face
indices consists of a tuple of three integer vertex indices.

The vertices themselves can be any type you want. For a 2D mesh, you might have
each vertex be simply a point:

    type alias Mesh2d =
        TriangularMesh Point2d

For a 3D mesh, each vertex might be a (point, normal) tuple:

    type alias Mesh3d =
        TriangularMesh ( Point3d, Vector3d )

In more complex cases, each vertex might be a record:

    type alias VertexData =
        { position : Point3d
        , normal : Vector3d
        , color : Color
        }

    type alias RenderMesh =
        TriangularMesh VertexData

-}
type TriangularMesh vertex
    = TriangularMesh { vertices : Array vertex, faceIndices : List ( Int, Int, Int ) }


{-| A mesh with no vertices or faces.
-}
empty : TriangularMesh vertex
empty =
    TriangularMesh { vertices = Array.empty, faceIndices = [] }


{-| Create a mesh from an array of vertices and list of face indices. For
example, to construct a square where `a` is the lower left corner, `b` is the
lower right corner, `c` is the upper right corner and `d` is the upper left
corner:

![Square mesh](https://ianmackenzie.github.io/elm-triangular-mesh/1.0.1/TriangularMesh/square.svg)

    vertices =
        Array.fromList [ a, b, c, d ]

    faceIndices =
        [ ( 0, 1, 2 ), ( 0, 2, 3 ) ]

    square =
        TriangularMesh.indexed vertices faceIndices

Invalid face indices (triples where any one of the three indices is out of
bounds) will be dropped.

-}
indexed : Array vertex -> List ( Int, Int, Int ) -> TriangularMesh vertex
indexed vertices_ faceIndices_ =
    let
        numVertices =
            Array.length vertices_

        validIndices ( i, j, k ) =
            (i >= 0 && i < numVertices)
                && (j >= 0 && j < numVertices)
                && (k >= 0 && k < numVertices)
    in
    if List.all validIndices faceIndices_ then
        TriangularMesh { vertices = vertices_, faceIndices = faceIndices_ }
    else
        TriangularMesh
            { vertices = vertices_
            , faceIndices = List.filter validIndices faceIndices_
            }


{-| Create a mesh from a list of triangular faces, where each face is given as a
tuple of three vertices. Note that this will not perform any kind of vertex
deduplication, so if any vertices are shared between different triangles then
they will occur more than once in the resulting mesh's vertex array.

    mesh =
        TriangleMesh.triangles [ ( a, b, c ), ( a, c, d ) ]

    TriangleMesh.vertices mesh
    --> Array.fromList [ a, b, c, a, c, d ]

    Array.faceIndices mesh
    --> [ ( 0, 1, 2 ), ( 3, 4, 5 ) ]

-}
triangles : List ( vertex, vertex, vertex ) -> TriangularMesh vertex
triangles faceVertices_ =
    TriangularMesh
        { vertices =
            faceVertices_
                |> List.map (\( v1, v2, v3 ) -> [ v1, v2, v3 ])
                |> List.concat
                |> Array.fromList
        , faceIndices =
            List.range 0 (List.length faceVertices_ - 1)
                |> List.map (\i -> ( 3 * i, 3 * i + 1, 3 * i + 2 ))
        }


{-| Create a fan-shaped mesh from the first given vertex to all vertices in the
given list. If the given list is empty or has only one element, then an empty
mesh is returned. Otherwise, the first face will be from the first given vertex
to the first and second list vertices, the second face will be from the first
given vertex to the second and third list vertices, etc.

    mesh =
        TriangleMesh.fan a [ b, c, d, e ]

    TriangleMesh.vertices mesh
    --> Array.fromList [ a, b, c, d, e ]

    TriangleMesh.faceIndices mesh
    --> [ ( 0, 1, 2 ), ( 0, 2, 3 ), ( 0, 3, 4 ) ]

    TriangleMesh.faceVertices mesh
    --> [ ( a, b, c ), ( a, c, d ), ( a, d, e ) ]

-}
fan : vertex -> List vertex -> TriangularMesh vertex
fan origin vertices_ =
    case vertices_ of
        first :: second :: rest ->
            TriangularMesh
                { vertices = Array.fromList (origin :: vertices_)
                , faceIndices =
                    List.range 0 (List.length rest)
                        |> List.map (\i -> ( 0, 1 + i, 2 + i ))
                }

        _ ->
            empty


{-| Create a strip-shaped mesh between two lists of vertices. The two lists
should be the same length; if one list is longer, the extra vertices will be
dropped. To get triangles with counterclockwise winding order, the second list
should be to the left of the first; for example, for two left-to-right vertex
lists, the second should be above the first.

    mesh =
        TriangleMesh.strip [ a, b, c ] [ d, e, f ]

    TriangleMesh.faceVertices mesh
    --> [ ( a, b, e )
    --> , ( a, e, d )
    --> , ( b, c, f )
    --> , ( b, f, e )
    --> ]

-}
strip : List vertex -> List vertex -> TriangularMesh vertex
strip bottom top =
    let
        pairs =
            List.map2 Tuple.pair bottom top

        numPairs =
            List.length pairs
    in
    if numPairs >= 2 then
        let
            vertices_ =
                pairs
                    |> List.map (\( a, b ) -> [ a, b ])
                    |> List.concat
                    |> Array.fromList

            faceIndices_ =
                List.range 0 (numPairs - 2)
                    |> List.map
                        (\i ->
                            [ ( 2 * i
                              , 2 * i + 2
                              , 2 * i + 3
                              )
                            , ( 2 * i
                              , 2 * i + 3
                              , 2 * i + 1
                              )
                            ]
                        )
                    |> List.concat
        in
        TriangularMesh { vertices = vertices_, faceIndices = faceIndices_ }
    else
        empty


{-| Get the vertices of a mesh.

    TriangularMesh.vertices square
    --> Array.fromList [ a, b, c, d ]

-}
vertices : TriangularMesh vertex -> Array vertex
vertices (TriangularMesh mesh) =
    mesh.vertices


{-| Get the faces of a mesh as triples of vertex indices.

    TriangularMesh.faceIndices square
    --> [ ( 0, 1, 2 ), ( 0, 2, 3 ) ]

-}
faceIndices : TriangularMesh vertex -> List ( Int, Int, Int )
faceIndices (TriangularMesh mesh) =
    mesh.faceIndices


{-| Get the faces of a mesh as triples of vertices.

    TriangularMesh.faceVertices square
    --> [ ( a, b, c ), ( a, c, d ) ]

-}
faceVertices : TriangularMesh vertex -> List ( vertex, vertex, vertex )
faceVertices mesh =
    let
        toFace ( i, j, k ) =
            Maybe.map3
                (\firstVertex secondVertex thirdVertex ->
                    ( firstVertex, secondVertex, thirdVertex )
                )
                (vertex i mesh)
                (vertex j mesh)
                (vertex k mesh)
    in
    List.filterMap toFace (faceIndices mesh)


{-| Get a particular vertex of a mesh by index. If the index is out of range,
returns `Nothing`.

    TriangularMesh.vertex 1 square
    --> Just b

    TriangularMesh.vertex 4 square
    --> Nothing

-}
vertex : Int -> TriangularMesh vertex -> Maybe vertex
vertex index mesh =
    Array.get index (vertices mesh)


canonicalize : Int -> Int -> ( Int, Int )
canonicalize i j =
    if i <= j then
        ( i, j )
    else
        ( j, i )


{-| Get all of the edges of a mesh as pairs of vertex indices. Each edge will
only be returned once, with the lower-index vertex listed first, and will be
returned in sorted order.

    TriangularMesh.edgeIndices square
    --> [ ( 0, 1 )
    --> , ( 0, 2 )
    --> , ( 0, 3 )
    --> , ( 1, 2 )
    --> , ( 2, 3 )
    --> ]

-}
edgeIndices : TriangularMesh vertex -> List ( Int, Int )
edgeIndices mesh =
    let
        addFace ( i, j, k ) edgeSet =
            edgeSet
                |> Set.insert (canonicalize i j)
                |> Set.insert (canonicalize j k)
                |> Set.insert (canonicalize k i)
    in
    List.foldl addFace Set.empty (faceIndices mesh) |> Set.toList


{-| Get all of the edges of a mesh as pairs of vertices. Each edge will only be
returned once, with the lower-index vertex listed first, and will be returned in
sorted order.

    TriangularMesh.edgeVertices square
    --> [ ( a, b )
    --> , ( a, c )
    --> , ( a, d )
    --> , ( b, c )
    --> , ( c, d )
    --> ]

-}
edgeVertices : TriangularMesh vertex -> List ( vertex, vertex )
edgeVertices mesh =
    let
        toEdge ( i, j ) =
            Maybe.map2 Tuple.pair (vertex i mesh) (vertex j mesh)
    in
    List.filterMap toEdge (edgeIndices mesh)


edgeFaceCounts : TriangularMesh vertex -> Dict ( Int, Int ) Int
edgeFaceCounts mesh =
    let
        increment count =
            case count of
                Just value ->
                    Just (value + 1)

                Nothing ->
                    Just 1

        add edgeIndices_ edgeDict =
            Dict.update edgeIndices_ increment edgeDict

        addEdges ( i, j, k ) edgeDict =
            edgeDict
                |> add (canonicalize i j)
                |> add (canonicalize j k)
                |> add (canonicalize k i)
    in
    List.foldl addEdges Dict.empty (faceIndices mesh)


openEdges : TriangularMesh vertex -> List ( vertex, vertex )
openEdges mesh =
    let
        prependTo accumulated firstVertex secondVertex =
            ( firstVertex, secondVertex ) :: accumulated

        prependIfOpen ( i, j ) edgeCount accumulated =
            if edgeCount == 1 then
                Maybe.map2 (prependTo accumulated)
                    (vertex i mesh)
                    (vertex j mesh)
                    |> Maybe.withDefault accumulated
            else
                accumulated
    in
    Dict.foldr prependIfOpen [] (edgeFaceCounts mesh)


{-| Transform a mesh by applying the given function to each of its vertices. For
example, if you had a 2D mesh where each vertex was an `( x, y )` tuple and you
wanted to convert it to a 3D mesh on the XY plane, you might use

    mesh2d : TriangularMesh ( Float, Float )
    mesh2d =
        ...

    to3d : ( Float, Float ) -> ( Float, Float, Float )
    to3d ( x, y ) =
        ( x, y, 0 )

    mesh3d : TriangularMesh ( Float, Float, Float )
    mesh3d =
        TriangularMesh.mapVertices to3d mesh2d

-}
mapVertices : (a -> b) -> TriangularMesh a -> TriangularMesh b
mapVertices function (TriangularMesh mesh) =
    TriangularMesh
        { vertices = Array.map function mesh.vertices
        , faceIndices = mesh.faceIndices
        }


reverseFaceIndices : TriangularMesh vertex -> TriangularMesh vertex
reverseFaceIndices (TriangularMesh mesh) =
    TriangularMesh
        { vertices = mesh.vertices
        , faceIndices = List.reverse mesh.faceIndices
        }


addMesh : TriangularMesh vertex -> TriangularMesh vertex -> TriangularMesh vertex
addMesh mesh accumulated =
    let
        meshVertices =
            vertices mesh

        meshFaceIndices =
            faceIndices mesh

        accumulatedVertices =
            vertices accumulated

        accumulatedFaceIndices =
            faceIndices accumulated

        offset =
            Array.length accumulatedVertices

        prependFace ( i, j, k ) accumulatedFaces =
            ( i + offset, j + offset, k + offset ) :: accumulatedFaces

        combinedFaceIndices =
            List.foldl prependFace accumulatedFaceIndices meshFaceIndices
    in
    TriangularMesh
        { vertices = Array.append accumulatedVertices meshVertices
        , faceIndices = combinedFaceIndices
        }


{-| Combine a list of meshes into a single mesh. This concatenates the vertex
arrays of each mesh and adjusts face indices to refer to the combined vertex
array.

    square =
        TriangularMesh.with
            { vertices = Array.fromList [ a, b, c, d ]
            , faceIndices = [ ( 0, 1, 2 ), ( 0, 2, 3 ) ]
            }

    triangle =
        TriangularMesh.with
            { vertices = Array.fromList [ e, f, g ]
            , faceIndices = [ ( 0, 1, 2 ) ]
            }

    TriangularMesh.combine [ square, triangle ]
    --> TriangularMesh.with
    -->     { vertices =
    -->         Array.fromList [ a, b, c, d, e, f, g ]
    -->     , faceIndices =
    -->         [ ( 0, 1, 2 )
    -->         , ( 0, 2, 3 )
    -->         , ( 4, 5, 6 )
    -->         ]
    -->     }

    TriangularMesh.combine [ triangle, square ]
    --> TriangularMesh.with
    -->     { vertices =
    -->         Array.fromList [ e, f, g, a, b, c, d ]
    -->     , faceIndices =
    -->         [ ( 0, 1, 2 )
    -->         , ( 3, 4, 5 )
    -->         , ( 3, 5, 6 )
    -->         ]
    -->     }

-}
combine : List (TriangularMesh a) -> TriangularMesh a
combine meshes =
    case meshes of
        first :: rest ->
            List.foldl addMesh (reverseFaceIndices first) rest
                |> reverseFaceIndices

        [] ->
            empty
