module OpenSolid.Mesh
    exposing
        ( Mesh
        , combine
        , edgeIndices
        , edges
        , empty
        , faceIndices
        , faces
        , mapVertices
        , vertex
        , vertices
        , with
        )

{-| This module provides functions for working with indexed triangular meshes.
You can:

  - Construct meshes from vertices and face indices
  - Extract vertices, faces and edges in various ways
  - Combine multiple meshes into a single mesh

Note that the `Array` type used by this package is the one from [`Array.Hamt`](http://package.elm-lang.org/packages/Skinney/elm-array-exploration/latest/Array-Hamt),
not Elm's built-in `Array` type.

@docs Mesh


# Constants

@docs empty


# Constructors

@docs with, combine


# Properties

@docs vertices, vertex, faceIndices, faces, edgeIndices, edges


# Transformations

@docs mapVertices

-}

import Array.Hamt as Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)


{-| A `Mesh` is a simple form of [face-vertex mesh](https://en.wikipedia.org/wiki/Polygon_mesh#Face-vertex_meshes)
defined by an array of vertices and a list of face indices. Each set of face
indices consists of a tuple of three integer vertex indices.

The vertices themselves can be any type you want, so you can use this for 2D or
3D meshes with any point type, store additional data such as normal vectors or
texture coordinates, etc.

-}
type Mesh vertex
    = Mesh { vertices : Array vertex, faceIndices : List ( Int, Int, Int ) }


{-| A mesh with no vertices or faces.
-}
empty : Mesh vertex
empty =
    with { vertices = Array.empty, faceIndices = [] }


{-| Construct a mesh directly from an array of vertices and list of face
indices. For example, to construct a square where `a` is the lower left
corner, `b` is the lower right corner, `c` is the upper right corner
and `d` is the upper left corner:

![Square mesh](https://opensolid.github.io/images/mesh/1.0/square.svg)

    square =
        Mesh.with
            { vertices = Array.fromList [ a, b, c, d ]
            , faceIndices = [ ( 0, 1, 2 ), ( 0, 2, 3 ) ]
            }

-}
with : { vertices : Array vertex, faceIndices : List ( Int, Int, Int ) } -> Mesh vertex
with =
    Mesh


{-| Get the vertices of a mesh.

    Mesh.vertices square
    --> Array.fromList [ a, b, c, d ]

-}
vertices : Mesh vertex -> Array vertex
vertices (Mesh { vertices }) =
    vertices


{-| Get the face indices of a mesh.

    Mesh.faceIndices square
    --> [ ( 0, 1, 2 ), ( 0, 2, 3 ) ]

-}
faceIndices : Mesh vertex -> List ( Int, Int, Int )
faceIndices (Mesh { faceIndices }) =
    faceIndices


{-| Get the faces of the mesh (by indexing into the array of vertices).

    Mesh.faces square
    --> [ ( a, b, c ), ( a, c, d ) ]

-}
faces : Mesh vertex -> List ( vertex, vertex, vertex )
faces mesh =
    let
        toFace ( i, j, k ) =
            Maybe.map3 (,,) (vertex i mesh) (vertex j mesh) (vertex k mesh)
    in
    List.filterMap toFace (faceIndices mesh)


{-| Get a particular vertex of a mesh by index. If the index is out of range,
returns `Nothing`.

    Mesh.vertex 1 square
    --> Just b

    Mesh.vertex 4 square
    --> Nothing

-}
vertex : Int -> Mesh vertex -> Maybe vertex
vertex index mesh =
    Array.get index (vertices mesh)


canonicalize : Int -> Int -> ( Int, Int )
canonicalize i j =
    if i <= j then
        ( i, j )
    else
        ( j, i )


{-| Get all of the edges of the mesh as pairs of vertex indices. Each edge will
only be returned once, with the lower-index vertex listed first, and will be
returned in sorted order.

    Mesh.edgeIndices square
    --> [ ( 0, 1 )
    --> , ( 0, 2 )
    --> , ( 0, 3 )
    --> , ( 1, 2 )
    --> , ( 2, 3 )
    --> ]

-}
edgeIndices : Mesh vertex -> Set ( Int, Int )
edgeIndices mesh =
    let
        addFace ( i, j, k ) edgeSet =
            edgeSet
                |> Set.insert (canonicalize i j)
                |> Set.insert (canonicalize j k)
                |> Set.insert (canonicalize k i)
    in
    List.foldl addFace Set.empty (faceIndices mesh)


{-| Get all of the edges of the mesh as pairs of vertices. Each edge will only
be returned once, with the lower-index vertex listed first, and will be returned
in sorted order.

    Mesh.edges square
    --> [ ( a, b )
    --> , ( a, c )
    --> , ( a, d )
    --> , ( b, c )
    --> , ( c, d )
    --> ]

-}
edges : Mesh vertex -> List ( vertex, vertex )
edges mesh =
    let
        toEdge ( i, j ) =
            Maybe.map2 (,) (vertex i mesh) (vertex j mesh)
    in
    List.filterMap toEdge (Set.toList (edgeIndices mesh))


edgeFaceCounts : Mesh vertex -> Dict ( Int, Int ) Int
edgeFaceCounts mesh =
    let
        increment count =
            case count of
                Just value ->
                    Just (value + 1)

                Nothing ->
                    Just 1

        add edgeIndices edgeDict =
            Dict.update edgeIndices increment edgeDict

        addEdges ( i, j, k ) edgeDict =
            edgeDict
                |> add (canonicalize i j)
                |> add (canonicalize j k)
                |> add (canonicalize k i)
    in
    List.foldl addEdges Dict.empty (faceIndices mesh)


openEdges : Mesh vertex -> List ( vertex, vertex )
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
example, to rotate a mesh of [`Point2d`](http://package.elm-lang.org/packages/opensolid/geometry/latest/OpenSolid-Point2d)
vertices:

    translatedMesh =
        mesh
            |> Mesh.mapVertices
                (Point2d.rotateAround centerPoint angle)

-}
mapVertices : (a -> b) -> Mesh a -> Mesh b
mapVertices function (Mesh { vertices, faceIndices }) =
    with
        { vertices = Array.map function vertices
        , faceIndices = faceIndices
        }


addMesh : Mesh vertex -> Mesh vertex -> Mesh vertex
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
    with
        { vertices = Array.append accumulatedVertices meshVertices
        , faceIndices = combinedFaceIndices
        }


{-| Combine a list of meshes into a single mesh. This concatenates the vertex
arrays of each mesh and adjusts face indices to refer to the combined vertex
array.
-}
combine : List (Mesh a) -> Mesh a
combine meshes =
    case meshes of
        first :: rest ->
            List.foldl addMesh first rest

        [] ->
            empty
