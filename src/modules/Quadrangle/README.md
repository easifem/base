# Quadrangle modules

| Module | Purpose |
| --- | --- |
| [ReferenceQuadrangle_Method](src/ReferenceQuadrangle_Method.F90) | Reference objects, element identifiers, connectivity, orientation and area |
| [QuadrangleInterpolationUtility](src/QuadrangleInterpolationUtility.F90) | Nodes, nodal coefficients, tensor and Dubiner bases, hierarchical modes and quadrature |

Public interface docblocks include argument and return shapes, formulas and
complete programs. [The examples index](examples/README.md) maps every public
name to an example with `Test_Method::OK` checks. Numerical implementations
remain unchanged in `src/submodules/Quadrangle`.

## Reference geometry and topology

Coordinates have point columns. UNIT vertices are `(0,0),(1,0),(1,1),(0,1)`;
BIUNIT vertices are `(-1,-1),(1,-1),(1,1),(-1,1)`. Constructors default to BIUNIT
and retain two intrinsic coordinate rows. Set `nsd` to describe the physical
embedding dimension and pass physical coordinates separately to geometry APIs.
Pointer constructors allocate a new target owned by the caller.

Element names such as `Quadrangle4` and `Quadrangle9` denote node counts,
rather than polynomial order. Lookup strings are case sensitive. Entity counts
are node, edge, face and cell counts; high order nodes are included in the first
entry. Metadata support for serendipity elements does not change the complete
tensor-product polynomial dimension.

Edge option 1 uses `[1,2],[4,3],[1,4],[2,3]`. Option 2 traverses
`[1,2],[2,3],[3,4],[4,1]`. `GetFaceConnectivity_Quadrangle` always forces option 2,
regardless of `opt`; `FacetConnectivity_Quadrangle` also returns option 2.
`FacetTopology_Quadrangle` uses the configured default edge option, so verify
that ordering before combining connectivity from different APIs.

`FaceShapeMetaData_Quadrangle` returns three orientation signs, unlike the
Triangle rotation/sign code. Its `localFaces` output holds cyclic vertex
identifiers. The oriented hierarchical overloads use the three-sign code with
four boundary edge signs.

`QuadArea2D` and `QuadrangleArea2D` are aliases and return signed area.
`QuadArea3D` and `QuadrangleArea3D` use the midpoint parallelogram construction.
The spatial example is planar and convex; it does not validate arbitrary warped
or self-intersecting quadrilaterals. Quality routines are placeholders returning
zero for every measure.

## Polynomial spaces and layouts

| Quantity | Dimension or layout |
| --- | --- |
| Tensor-product space | `(p+1)(q+1)` modes |
| Tensor interior modes | `(p-1)(q-1)` for positive orders |
| Dubiner total-degree space | `(p+1)(p+2)/2` modes |
| Exponent table | Monomial rows, two coordinate columns |
| Basis values | Point rows, basis columns |
| Gradients | Point, basis, coordinate axes |
| Quadrature | Coordinate rows, final weight row; point columns |

`TensorProdBasis_Quadrangle` and `OrthogonalBasis_Quadrangle` are aliases.
The first directional polynomial degree varies fastest across basis columns.
Separate `x,y` overloads create a tensor grid of points; paired `xij` stores
one point per column. **Current tensor values and gradients use `basisType1`
in both directions and ignore `basisType2`.** The examples use the same family
in both directions. Directional family parameters remain separate.

Dubiner routines evaluate total-degree collapsed-coordinate polynomials; they
are not the complete quadrilateral tensor-product basis. Modes are ordered
`(0,0),(0,1),...,(1,0),...`. The examples check the constant mode and the first
nonconstant Jacobi mode and its derivative.

The API retains the spelling `Heirarchical`. Hierarchical modes are ordered as
vertices, bottom/right/top/left boundary edges, then cell modes. Cell orders
`pb,qb` can differ from edge orders `pe3,pe4,qe1,qe2`. These APIs use BIUNIT
coordinates. Separate `x,y` inputs to vertex/edge/cell entity routines are
**paired vectors**, unlike the tensor-grid overloads above.

Interpolation defaults to BIUNIT. `VEFC` places vertices before edge and cell
nodes; grid reorderers allow clockwise or anticlockwise traversal and a starting
corner. Supply vertex coordinates to map nodes to another quadrilateral.

Lagrange coefficient columns satisfy `V*C=I`. Keep query points separate from
the INOUT node array. Initialize `firstCall=.TRUE.` to build a coefficient cache,
then explicitly use `.FALSE.` only with the same nodes, degree and family.

## Quadrature and validation

`order` requests polynomial integration degree; directional `p,q` specify the
requested degrees separately. `QuadratureNumber_Quadrangle` returns two
one-dimensional point counts. Explicit `nipsx,nipsy` are length-one arrays.
Weights include the selected family weights and geometric Jacobian when physical
vertices are supplied. Their sum is four on BIUNIT or one on UNIT for unweighted
Gauss integration. Keep point and weight columns paired.

The shared [FORMAL project](../../../doc/formal.md) includes this directory.
Generate from the project root:

```sh
formal generate --project doc/formal.md --mirror-sources
```

Read the generated
[Quadrangle reference](../../../docs/api/src/modules/Quadrangle/src/QuadrangleInterpolationUtility.md).
Building, installing and executing examples require explicit permission under
`AGENTS.md`. Once authorized:

```sh
easifem install base --no-download
cd src/modules/Quadrangle/examples
easifem run QuadrangleTensorBasis_test_1.F90
```
