module OpenSolid.Mesh
    exposing
        ( Mesh
        , combine
        , edgeIndices
        , edges
        , empty
        , faceIndices
        , faces
        , fromArray
        , fromList
        , map
        , openEdgeIndices
        , openEdges
        , vertex
        , vertices
        )

import Array.Hamt as Array exposing (Array)
import Dict
import Set


type Mesh vertex
    = Mesh (Array vertex) (List ( Int, Int, Int ))


empty : Mesh vertex
empty =
    Mesh Array.empty []


fromList : List vertex -> List ( Int, Int, Int ) -> Mesh vertex
fromList vertexList faceIndices =
    Mesh (Array.fromList vertexList) faceIndices


fromArray : Array vertex -> List ( Int, Int, Int ) -> Mesh vertex
fromArray vertexArray faceIndices =
    Mesh vertexArray faceIndices


vertices : Mesh vertex -> Array vertex
vertices (Mesh vertices _) =
    vertices


faceIndices : Mesh vertex -> List ( Int, Int, Int )
faceIndices (Mesh _ faceIndices) =
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


edgeIndices : Mesh vertex -> List ( Int, Int )
edgeIndices mesh =
    let
        addFace ( i, j, k ) edgeSet =
            edgeSet
                |> Set.insert (canonicalize i j)
                |> Set.insert (canonicalize j k)
                |> Set.insert (canonicalize k i)
    in
    List.foldl addFace Set.empty (faceIndices mesh) |> Set.toList


edges : Mesh vertex -> List ( vertex, vertex )
edges mesh =
    let
        toEdge ( i, j ) =
            Maybe.map2 (,) (vertex i mesh) (vertex j mesh)
    in
    List.filterMap toEdge (edgeIndices mesh)


openEdgeIndices : Mesh vertex -> List ( Int, Int )
openEdgeIndices mesh =
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

        edgeDict =
            List.foldl addEdges Dict.empty (faceIndices mesh)

        prependIfOpen edgeIndices edgeCount accumulated =
            if edgeCount == 1 then
                edgeIndices :: accumulated
            else
                accumulated
    in
    Dict.foldr prependIfOpen [] edgeDict


openEdges : Mesh vertex -> List ( vertex, vertex )
openEdges mesh =
    let
        toEdge ( i, j ) =
            Maybe.map2 (,) (vertex i mesh) (vertex j mesh)
    in
    List.filterMap toEdge (openEdgeIndices mesh)


mapVertices : (a -> b) -> Mesh a -> Mesh b
mapVertices function (Mesh vertices faceIndices) =
    Mesh (Array.map function vertices) faceIndices


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
    Mesh (Array.append firstVertices secondVertices) combinedFaceIndices


combine : List (Mesh a) -> Mesh a
combine meshes =
    case meshes of
        first :: rest ->
            List.foldl appendTo first rest

        [] ->
            empty
