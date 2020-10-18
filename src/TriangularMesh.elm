module TriangularMesh exposing
    ( TriangularMesh
    , empty
    , indexed, triangles, fan, radial, strip, grid
    , tube, ring, ball
    , indexedGrid, indexedTube, indexedRing, indexedBall
    , combine
    , vertices, vertex, faceIndices, faceVertices, edgeIndices, edgeVertices
    , mapVertices
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

@docs indexed, triangles, fan, radial, strip, grid


## Special grids

These functions work similarly to `grid` but let you construct shapes like
cylindrical tubes, spheres or toruses, where some edges of the grid 'wrap
around' and join up with other edges. For example, a cylindrical tube (a
cylinder without ends) can be thought of as a piece of paper curled around so
that one edge touches the other.

These functions ensure that in cases like a cylindrical tube, there's actually
only _one_ set of vertices along the shared edge that is then referenced by the
faces on either side. Roughly speaking, this is the difference between a
polyline where the last vertex happens to be the same as the first (and so looks
like a closed polygon, but isn't actually connected) and a proper polygon where
the last vertex is actually connected back to the first.

Note that these functions _can_ be used to create meshes that represent actual
cylinders, spheres, and toruses, but they can also be used to make any mesh that
is [topologically](https://en.wikipedia.org/wiki/Topology) equivalent to one of
those. For example, the `ball` function can be used to create meshes for both
spheres and [ellipsoids](https://en.wikipedia.org/wiki/Ellipsoid).

@docs tube, ring, ball


## Indexed grids

These functions work like their non-`indexed` versions, but the function gets
passed the _indices_ of individual vertices instead of their parameter values.
For example, given some function `f` that creates vertices,

    TriangularMesh.indexedGrid 3 2 f

will produce a mesh like this:

![Rectangular mesh](https://ianmackenzie.github.io/elm-triangular-mesh/1.1.0/indexedGrid.png)

@docs indexedGrid, indexedTube, indexedRing, indexedBall


# Combining meshes

@docs combine


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
        TriangularMesh.triangles [ ( a, b, c ), ( a, c, d ) ]

    TriangularMesh.vertices mesh
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
        TriangularMesh.fan a [ b, c, d, e ]

    TriangularMesh.vertices mesh
    --> Array.fromList [ a, b, c, d, e ]

    TriangularMesh.faceVertices mesh
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


{-| Like `fan`, but also connect the last vertex in the list back to the first.
This can be useful to create cones or cone-like shapes with a tip vertex
connected to a closed loop of other vertices.

    mesh =
        TriangularMesh.radial a [ b, c, d, e ]

    TriangularMesh.vertices mesh
    --> Array.fromList [ a, b, c, d, e ]

    TriangularMesh.faceVertices mesh
    --> [ ( a, b, c )
    --> , ( a, c, d )
    --> , ( a, d, e )
    --> , ( a, e, b )
    --> ]

-}
radial : vertex -> List vertex -> TriangularMesh vertex
radial tip loop =
    let
        n =
            List.length loop
    in
    if n >= 2 then
        TriangularMesh
            { vertices = Array.fromList (tip :: loop)
            , faceIndices =
                List.range 0 (n - 1)
                    |> List.map
                        (\i ->
                            if i == n - 1 then
                                ( 0, n, 1 )

                            else
                                ( 0, 1 + i, 2 + i )
                        )
            }

    else
        empty


{-| Create a strip-shaped mesh between two lists of vertices. The two lists
should be the same length; if one list is longer, the extra vertices will be
dropped. To get triangles with counterclockwise winding order, the second list
should be to the left of the first; for example, for two left-to-right vertex
lists, the second should be above the first.

    mesh =
        TriangularMesh.strip [ a, b, c ] [ d, e, f ]

    TriangularMesh.faceVertices mesh
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


gridFaceIndices : Int -> Int -> Int -> Int -> Int -> List ( Int, Int, Int ) -> List ( Int, Int, Int )
gridFaceIndices uSteps uVertices vVertices uIndex0 vIndex0 accumulatedIndices =
    let
        rowStart0 =
            uVertices * vIndex0

        rowStart1 =
            uVertices * ((vIndex0 + 1) |> modBy vVertices)

        uIndex1 =
            (uIndex0 + 1) |> modBy uVertices

        index00 =
            rowStart0 + uIndex0

        index10 =
            rowStart0 + uIndex1

        index01 =
            rowStart1 + uIndex0

        index11 =
            rowStart1 + uIndex1

        lowerFaceIndices =
            ( index00, index10, index11 )

        upperFaceIndices =
            ( index00, index11, index01 )

        updatedIndices =
            lowerFaceIndices :: upperFaceIndices :: accumulatedIndices
    in
    if uIndex0 > 0 then
        gridFaceIndices uSteps uVertices vVertices (uIndex0 - 1) vIndex0 updatedIndices

    else if vIndex0 > 0 then
        gridFaceIndices uSteps uVertices vVertices (uSteps - 1) (vIndex0 - 1) updatedIndices

    else
        updatedIndices


gridImpl : Int -> Int -> Int -> Int -> (Int -> Int -> vertex) -> TriangularMesh vertex
gridImpl uSteps vSteps uVertices vVertices function =
    if uVertices <= 1 || vVertices <= 1 then
        empty

    else
        TriangularMesh
            { vertices =
                Array.initialize (uVertices * vVertices) <|
                    \index -> function (index |> modBy uVertices) (index // uVertices)
            , faceIndices =
                gridFaceIndices uSteps uVertices vVertices (uSteps - 1) (vSteps - 1) []
            }


toIndexedFunction : Int -> Int -> (Float -> Float -> vertex) -> Int -> Int -> vertex
toIndexedFunction uSteps vSteps function uIndex vIndex =
    function (toFloat uIndex / toFloat uSteps) (toFloat vIndex / toFloat vSteps)


{-| -}
indexedGrid : Int -> Int -> (Int -> Int -> vertex) -> TriangularMesh vertex
indexedGrid uSteps vSteps function =
    gridImpl uSteps vSteps (uSteps + 1) (vSteps + 1) function


{-| Construct a mesh in the form of a rectangular grid. This is useful for
constructing things like terrain meshes given a height function, or a
[parametric surface](https://services.math.duke.edu/education/ccp/materials/mvcalc/parasurfs/para1.html)
given a function that computes a 3D point (and perhaps a normal vector) from
U and V parameter values.

The arguments are the number of steps to take in the U and V directions, and a
function that takes U and V values (which each range between 0 and 1) and
returns some sort of vertex value. A mesh will then be constructed will all
vertices correctly connected to each other. For example, given some function `f`
that creates vertices,

    TriangularMesh.grid 3 2 f

will produce a mesh like this:

![Rectangular mesh](https://ianmackenzie.github.io/elm-triangular-mesh/1.1.0/grid.png)

-}
grid : Int -> Int -> (Float -> Float -> vertex) -> TriangularMesh vertex
grid uSteps vSteps function =
    indexedGrid uSteps vSteps (toIndexedFunction uSteps vSteps function)


{-| -}
indexedTube : Int -> Int -> (Int -> Int -> vertex) -> TriangularMesh vertex
indexedTube uSteps vSteps function =
    gridImpl uSteps vSteps (uSteps + 1) vSteps function


{-| Construct a mesh that is topologically equivalent to a cylinder, where the
U parameter value is along the axis of the sphere and the V parameter value is
around the circumference. The mesh will wrap in the V direction, so the provided
function will never be called with V=1; instead, the last vertices in the V
direction will connect back to the first ones.

If you wanted to construct a 5 meter long, 2 meter radius cylindrical mesh along
the X axis, you might do something like

    import TriangularMesh

    TriangularMesh.tube 1 72 <|
        \u v ->
            let
                theta =
                    2 * pi * v
            in
            { x = 5 * u
            , y = 2 * sin theta
            , z = 2 * cos theta
            }

-}
tube : Int -> Int -> (Float -> Float -> vertex) -> TriangularMesh vertex
tube uSteps vSteps function =
    indexedTube uSteps vSteps (toIndexedFunction uSteps vSteps function)


{-| -}
indexedRing : Int -> Int -> (Int -> Int -> vertex) -> TriangularMesh vertex
indexedRing uSteps vSteps function =
    gridImpl uSteps vSteps uSteps vSteps function


{-| Construct a mesh that is topologically equivalent to a [torus](https://en.wikipedia.org/wiki/Torus).
This is similar to `tube` except that the mesh wraps in both the U _and_ V
directions.
-}
ring : Int -> Int -> (Float -> Float -> vertex) -> TriangularMesh vertex
ring uSteps vSteps function =
    indexedRing uSteps vSteps (toIndexedFunction uSteps vSteps function)


ballGridIndices : Int -> Int -> Int -> List ( Int, Int, Int ) -> List ( Int, Int, Int )
ballGridIndices uSteps uIndex0 vIndex0 accumulatedIndices =
    let
        uIndex1 =
            (uIndex0 + 1) |> modBy uSteps

        rowStart0 =
            2 + uSteps * (vIndex0 - 1)

        rowStart1 =
            rowStart0 + uSteps

        index00 =
            rowStart0 + uIndex0

        index10 =
            rowStart0 + uIndex1

        index01 =
            rowStart1 + uIndex0

        index11 =
            rowStart1 + uIndex1

        lowerFaceIndices =
            ( index00, index10, index11 )

        upperFaceIndices =
            ( index00, index11, index01 )

        updatedIndices =
            lowerFaceIndices :: upperFaceIndices :: accumulatedIndices
    in
    if uIndex0 > 0 then
        ballGridIndices uSteps (uIndex0 - 1) vIndex0 updatedIndices

    else if vIndex0 > 1 then
        ballGridIndices uSteps (uSteps - 1) (vIndex0 - 1) updatedIndices

    else
        updatedIndices


addBallBottomIndices : Int -> Int -> List ( Int, Int, Int ) -> List ( Int, Int, Int )
addBallBottomIndices uSteps uIndex0 accumulatedIndices =
    let
        uIndex1 =
            (uIndex0 + 1) |> modBy uSteps

        updatedIndices =
            ( 0, 2 + uIndex1, 2 + uIndex0 ) :: accumulatedIndices
    in
    if uIndex0 > 0 then
        addBallBottomIndices uSteps (uIndex0 - 1) updatedIndices

    else
        updatedIndices


addBallTopIndices : Int -> Int -> Int -> List ( Int, Int, Int ) -> List ( Int, Int, Int )
addBallTopIndices uSteps vSteps uIndex0 accumulatedIndices =
    let
        uIndex1 =
            (uIndex0 + 1) |> modBy uSteps

        rowStart =
            2 + uSteps * (vSteps - 2)

        updatedIndices =
            ( rowStart + uIndex0, rowStart + uIndex1, 1 ) :: accumulatedIndices
    in
    if uIndex0 > 0 then
        addBallTopIndices uSteps vSteps (uIndex0 - 1) updatedIndices

    else
        updatedIndices


{-| -}
indexedBall : Int -> Int -> (Int -> Int -> vertex) -> TriangularMesh vertex
indexedBall uSteps vSteps function =
    if uSteps < 2 || vSteps < 2 then
        empty

    else
        let
            gridIndices =
                if vSteps > 2 then
                    ballGridIndices uSteps (uSteps - 1) (vSteps - 2) []

                else
                    []
        in
        TriangularMesh
            { vertices =
                Array.initialize (2 + uSteps * (vSteps - 1)) <|
                    \index ->
                        if index >= 2 then
                            let
                                k =
                                    index - 2

                                uIndex =
                                    k |> modBy uSteps

                                vIndex =
                                    1 + k // uSteps
                            in
                            function uIndex vIndex

                        else if index == 1 then
                            function 0 vSteps

                        else
                            function 0 0
            , faceIndices =
                gridIndices
                    |> addBallBottomIndices uSteps (uSteps - 1)
                    |> addBallTopIndices uSteps vSteps (uSteps - 1)
            }


{-| Construct a mesh that is topologically equivalent to a sphere; the
resulting mesh will have the basic sphere structure as shown [here](http://www.songho.ca/opengl/gl_sphere.html)
with U corresponding to Θ and V corresponding to Φ (with V=0 meaning the bottom
point on the sphere or 'south pole' and V=1 meaning the top point on the
sphere or 'north pole').
-}
ball : Int -> Int -> (Float -> Float -> vertex) -> TriangularMesh vertex
ball uSteps vSteps function =
    indexedBall uSteps vSteps (toIndexedFunction uSteps vSteps function)


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
