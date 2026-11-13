# Triangle modules

| Module | Role |
| --- | --- |
| [Triangle_Method](src/Triangle_Method.F90) | Coordinate based geometry kernels adapted from John Burkardt |
| [ReferenceTriangle_Method](src/ReferenceTriangle_Method.F90) | Reference objects, topology, element metadata and geometry wrappers |
| [TriangleInterpolationUtility](src/TriangleInterpolationUtility.F90) | Nodal, Dubiner and hierarchical bases, nodes and quadrature |

Public API docblocks contain argument and return shapes, mathematical relations
and complete runnable programs. [The examples index](examples/README.md) maps
public names to programs with `Test_Method::OK` checks. The existing
`HierarchicalEdgeGradient_test_1.F90` regression is preserved.

## Coordinate and geometry conventions

Vertex and evaluation point coordinates are columns of arrays. A planar triangle
has shape `(2,3)` and a spatial triangle `(3,3)`. The unit reference vertices are
`(0,0),(1,0),(0,1)`; biunit vertices are `(-1,-1),(1,-1),(-1,1)`.
`ReferenceTriangle_` stores two intrinsic coordinate rows and records `nsd`
separately. Its constructor defaults to `UNIT`; specify the domain explicitly.
Pointer constructors allocate a target owned by the caller.

Planar area is signed, with positive area for counterclockwise vertices. Spatial
area is nonnegative. Angles are in radians. Barycentric coordinates reconstruct
a point and sum to one. Containment variant 1 uses barycentric coordinates,
variant 2 requires counterclockwise ordering, and variant 3 accepts either
orientation for nondegenerate triangles.

Distance and nearest point APIs measure distance to the **boundary edges**,
including when the query lies inside the filled triangle. The signed planar
distance is negative inside a counterclockwise triangle. Line intersection uses
an infinite line: in the parametric form the second argument is a direction,
while in explicit form it is another point.

Many ReferenceTriangle geometry wrappers use only the first two coordinate rows.
`CircumCircle` and `Incircle` return `(radius, centerX, centerY, 0)`, rather than
center coordinates followed by a radius. Wrapper output shapes are fixed even
for planar geometry; for example, centers and centroids contain three entries.

## Element topology

Element names such as `Triangle3` and `Triangle6` denote node counts. Metadata
lookup strings are case sensitive. Node, edge, face and cell counts are stored
in that order; the node count includes high order nodes.

Connectivity option 1 uses edges `[1,2], [1,3], [2,3]`, and option 2 uses
`[1,2], [2,3], [3,1]`. `GetFaceConnectivity_Triangle` currently forces option 2,
regardless of its `opt` argument. `FacetConnectivity_Triangle` also returns the
option 2 vertex connectivity. Keep basis orientation conventions consistent
with the chosen connectivity.

`FaceShapeMetaData_Triangle` returns a two entry orientation code: a zero based
rotation and a sign. Its `localFaces` output contains cyclic vertex identifiers,
rather than a permutation of local indices.

## Interpolation and quadrature

For scalar polynomial degree `p`,

$$N=(p+1)(p+2)/2,\qquad N_{\mathrm{interior}}=(p-1)(p-2)/2.$$

Use positive degrees with the DOF helpers. Exponent tables have monomial rows
and coordinate columns. Coordinate arrays have point columns; basis output
arrays have point rows and basis columns. Gradients append a final coordinate
direction axis of length two.

Nodal Lagrange coefficients satisfy `V*C=I`. Cache coefficients only for the
same degree, nodes, domain and basis family. Evaluation nodes are INOUT, so use
a separate array for query points. `firstCall` controls cache construction;
initialize it explicitly.

`Dubiner_Triangle` and `OrthogonalBasis_Triangle` are aliases. Total degree
modes are ordered `(0,0),(0,1),...,(1,0),...`. The separate `x,y` overload forms
a tensor grid in collapsed square coordinates, rather than pairing coordinates.
The paired point examples use interior points to avoid the collapsed apex.

The historical spelling **Heirarchical** is part of the API. Hierarchical modes
are ordered as vertices, boundary edges, then cell modes. Edge orders can differ;
cell mode count is `(p-1)(p-2)/2`. Oriented subroutine overloads accept three edge
signs and the rotation/sign face code. Vertex modes are barycentric coordinates;
edge and cell modes vanish at the vertices.

Quadrature output has coordinate rows followed by a weight row, with points in
columns. `order` requests polynomial integration degree; `nips` requests a
supported explicit count. Economical rules use the Solin tables, with a tensor
fallback for unavailable orders. `QuadratureNumber_Triangle` currently ignores
`quadType`. The weights sum to `1/2` on UNIT and `2` on BIUNIT. Supplying physical
vertices maps points and scales weights by the geometric Jacobian.

## Current implementation limits

- `HighorderElement_Triangle` allocates three face columns but accesses a fourth.
  Its example uses interpolation node generation directly instead of calling
  the unsafe constructor.
- `triangle_diameter_2d` squares sums of coordinate differences, rather than
  summing their squares. Its example records the existing result and explicitly
  identifies the disagreement with the geometric diameter. `Diameter` inherits
  the same problem.
- `triangle_area_vector_3d` returns the full edge cross product, whose norm is
  twice the area. `ArealVector` returns signed area in its 2D branch but the
  full cross product in 3D, so its normalization depends on embedding dimension.
- The orthogonal gradient implementation uses UNIT chain factors even for BIUNIT
  input. Its new examples use UNIT; BIUNIT gradient scaling needs review.
- Several geometry wrappers are planar even when a reference object's `nsd` is
  three. Use the explicit spatial kernels where their contracts are appropriate.
- Sampling interfaces accept a seed as input and do not return its advanced state.

The source implementations are unchanged. Known behavior examples and
workarounds are labeled so they do not imply numerical correctness of an
affected routine.

## Generate and validate

The shared [FORMAL project](../../../doc/formal.md) includes these modules.
From the project root run:

```sh
formal generate --project doc/formal.md --mirror-sources
```

Read the generated
[Triangle reference](../../../docs/api/src/modules/Triangle/src/TriangleInterpolationUtility.md).

Building, installing and running examples require explicit permission under
`AGENTS.md`. Once authorized:

```sh
easifem install base --no-download
cd src/modules/Triangle/examples
easifem run triangle_area_2d_test_1.F90
```
