# Tetrahedron modules

| Module | Purpose |
| --- | --- |
| [ReferenceTetrahedron_Method](src/ReferenceTetrahedron_Method.F90) | Reference objects, element metadata, connectivity and volume |
| [TetrahedronInterpolationUtility](src/TetrahedronInterpolationUtility.F90) | Nodal, barycentric, hierarchical and orthogonal bases, derivatives and quadrature |

Public interface docblocks include argument and result shapes, formulas and
complete programs. [The examples index](examples/README.md) maps all public
names to programs with `Test_Method::OK` checks. Numerical implementations are
unchanged. Examples that work around known limitations say so explicitly.

## Reference geometry and connectivity

Use `nsd=3`. Coordinates have point columns. UNIT vertices are the origin and
the three coordinate unit vectors. BIUNIT vertices are
`(-1,-1,-1),(1,-1,-1),(-1,1,-1),(-1,-1,1)`. Constructors default to UNIT; set the
domain explicitly for reproducibility. Pointer constructors allocate a target
owned by the caller.

Element names such as `Tetrahedron4` and `Tetrahedron10` denote node counts,
not interpolation degrees. Lookup strings are case sensitive. Entity counts
store node, edge, face and cell counts, including high order nodes.

The edge order is `[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]`.
Nodal facets use `[1,3,2],[1,2,4],[1,4,3],[2,3,4]`.
`FacetConnectivity_Tetrahedron` uses `[1,2,3],[1,2,4],[1,3,4],[2,3,4]` for its
hierarchy and orthogonal family selectors. Both family strings are required;
keep the selected basis facet ordering consistent with the reference topology.

Volume is unsigned:

$$V=|\det[v_2-v_1,v_3-v_1,v_4-v_1]|/6.$$

The reference volume is `1/6` on UNIT and `4/3` on BIUNIT. Quality APIs currently
return a placeholder zero, rather than an implemented mesh quality measure.

## Polynomial spaces and data layouts

For degree `p`, the scalar polynomial dimension is

$$N=(p+1)(p+2)(p+3)/6.$$

Hierarchical entity counts are

$$N_v=4,\quad N_e=6(p-1),\quad
N_f=4(p-1)(p-2)/2,\quad N_c=(p-1)(p-2)(p-3)/6.$$

Use positive orders. Edge modes begin at degree two, facet modes at three, and
cell modes at four. Separate entity order overloads allow different edge and
facet orders. The low-order polynomial count formulas are not general guarded
combinatorial functions outside their valid degree ranges.

| Data | Layout |
| --- | --- |
| Cartesian coordinates | Three rows, points in columns |
| Barycentric coordinates | Four rows, points in columns; each column sums to one |
| Exponent table | Monomial rows, three coordinate columns |
| Basis values | Point rows, basis columns |
| Cartesian gradients | Point, basis, three coordinate directions |
| Quadrature | Three coordinate rows and a final weight row; point columns |

Barycentric vertex functions are the barycentric coordinates. Edge, facet and
cell functions vanish at vertices. Combined hierarchical outputs contain
vertices, edges, facets and cell modes in that order. The public spelling
`Heirarchical` is retained. Precomputed `phi` overloads expect kernel degrees
indexed from zero and row blocks of the appropriate barycentric differences.

Nodal interpolation defaults to UNIT and uses the recursive node builder.
Use the exact layout string `VEFC` for entity reordering; that comparison is
case sensitive. Supplying four physical vertex columns maps generated nodes.
The recursive builder uses fixed internal work arrays; do not assume that
arbitrarily high polynomial orders are supported.

Lagrange coefficient columns satisfy `V*C=I`. Use separate query and INOUT node
arrays. Reuse a coefficient cache only for the same nodes, degree, family and
reference domain, explicitly controlling `firstCall`.

The orthogonal basis uses collapsed coordinates and total-degree modes ordered
with the third degree varying fastest, then the second, then the first.
Separate `x,y,z` overloads evaluate tensor grids in collapsed cube coordinates.
The paired-point examples use interior points to avoid singular collapsed
coordinate denominators.

## Current implementation limits

- The uniform-order subroutine specific
  `HeirarchicalBasisGradient_Tetrahedron2_` is declared but has no implementation.
  Call the generic subroutine with all six edge orders and four facet orders
  explicitly, as in the new example. The uniform-order function wrapper exists.
- Both orthogonal analytic gradient implementations write `ans0(:,:,2)` twice
  and leave `ans0(:,:,3)` unfilled in the modal derivative loop. Their example
  uses finite differences of the working value API and does not call or validate
  the affected analytic gradients. Selecting an orthogonal backend in another
  gradient dispatcher inherits this problem.
- Reference face connectivity fills all face nodes only for orders one and two.
  Higher order metadata does not imply complete topology support. The higher
  order element construction example uses quadratic order.
- Quality routines return zero for every selector.
- Internal legacy node routines and under-development alternatives are private;
  the examples use the current public recursive interpolation path.

## Quadrature and validation

`order` requests polynomial integration degree. `nips=[n]` requests an explicit
supported point count from the economical Solin tables. Larger orders can use
a tensor fallback. The economical count selector currently ignores `quadType`.
Physical vertices map coordinates and scale weights by the geometric Jacobian.
For unweighted integration the weights sum to the element volume. The example
also checks an exact linear moment.

The shared [FORMAL project](../../../doc/formal.md) includes this directory.
From the project root run:

```sh
formal generate --project doc/formal.md --mirror-sources
```

Read the generated
[Tetrahedron reference](../../../docs/api/src/modules/Tetrahedron/src/TetrahedronInterpolationUtility.md).
Building, installing and executing examples require explicit permission under
`AGENTS.md`. Once authorized:

```sh
easifem install base --no-download
cd src/modules/Tetrahedron/examples
easifem run TetrahedronVolume3D_test_1.F90
```
