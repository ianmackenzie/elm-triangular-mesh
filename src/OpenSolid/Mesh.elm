module OpenSolid.Mesh
    exposing
        ( Mesh
        , combine
        , edgeFaceCounts
        , edgeIndices
        , edges
        , empty
        , faceIndices
        , faces
        , mapVertices
        , openEdges
        , vertex
        , vertices
        , with
        )

import Array.Hamt as Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)


type Mesh vertex
    = Mesh { vertices : Array vertex, faceIndices : List ( Int, Int, Int ) }


empty : Mesh vertex
empty =
    with { vertices = Array.empty, faceIndices = [] }


with : { vertices : Array vertex, faceIndices : List ( Int, Int, Int ) } -> Mesh vertex
with =
    Mesh


vertices : Mesh vertex -> Array vertex
vertices (Mesh { vertices }) =
    vertices


faceIndices : Mesh vertex -> List ( Int, Int, Int )
faceIndices (Mesh { faceIndices }) =
    faceIndices


faces : Mesh vertex -> List ( vertex, vertex, vertex )
faces mesh =
    let
        toFace ( i, j, k ) =
            Maybe.map3 (,,) (vertex i mesh) (vertex j mesh) (vertex k mesh)
    in
    List.filterMap toFace (faceIndices mesh)


vertex : Int -> Mesh vertex -> Maybe vertex
vertex index mesh =
    Array.get index (vertices mesh)


canonicalize : Int -> Int -> ( Int, Int )
canonicalize i j =
    if i <= j then
        ( i, j )
    else
        ( j, i )


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


mapVertices : (a -> b) -> Mesh a -> Mesh b
mapVertices function (Mesh { vertices, faceIndices }) =
    with
        { vertices = Array.map function vertices
        , faceIndices = faceIndices
        }


appendTo : Mesh vertex -> Mesh vertex -> Mesh vertex
appendTo firstMesh secondMesh =
    let
        firstVertices =
            vertices firstMesh

        firstFaceIndices =
            faceIndices firstMesh

        secondVertices =
            vertices secondMesh

        secondFaceIndices =
            faceIndices secondMesh

        offset =
            Array.length firstVertices

        prependFace ( i, j, k ) faces =
            ( i + offset, j + offset, k + offset ) :: faces

        combinedFaceIndices =
            List.foldl prependFace firstFaceIndices secondFaceIndices
    in
    with
        { vertices = Array.append firstVertices secondVertices
        , faceIndices = combinedFaceIndices
        }


combine : List (Mesh a) -> Mesh a
combine meshes =
    case meshes of
        first :: rest ->
            List.foldl appendTo first rest

        [] ->
            empty
