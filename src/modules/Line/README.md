# Line modules

This directory contains three complementary APIs:

| Module | Role |
| --- | --- |
| [Line_Method](src/Line_Method.F90) | Explicit and implicit lines, intersections, projections and finite segment distances |
| [ReferenceLine_Method](src/ReferenceLine_Method.F90) | Reference objects, element metadata, connectivity and higher order elements |
| [LineInterpolationUtility](src/LineInterpolationUtility.F90) | Nodes, nodal and modal bases, derivatives and quadrature |

The complete API docblocks include argument types and shapes, return values,
formulas and runnable programs. [The examples index](examples/README.md) maps
public names to standalone programs, each with `Test_Method::OK` checks.

## Geometry and topology conventions

An explicit infinite line is specified by two distinct endpoints. An implicit
planar line is `a*x+b*y+c=0`; it is degenerate when both `a` and `b` vanish.
Segment distance and nearest point routines clamp the line parameter to `[0,1]`.
Signed infinite line distance is negative to the left of the oriented line.

Intersection status codes are `1` for a unique point, `0` for no intersection,
and `2` for coincident lines. Implicit intersections additionally return `-1`
or `-2` for a degenerate first or second input. Consult the specific routine's
contract when inputs can coincide or become degenerate.

`ReferenceLine_` stores one intrinsic coordinate row. `nsd` records the physical
embedding dimension; it does not enlarge the reference coordinate array.
Constructors default to `BIUNIT`, with endpoints `-1,+1`; specify `domainName`
explicitly to choose `UNIT`, with endpoints `0,1`. Pointer constructors allocate
a new target that belongs to the caller.

Element names such as `Line2` and `Line3` denote node counts, not polynomial
orders. `Line2` is linear. Name lookup strings are case sensitive.
`TotalEntities_Line` stores node, edge, face and cell counts in that order;
its node count includes high order nodes. Line facets are the two endpoints.
Use a fresh output object for `HighOrderElement_Line`.

## Interpolation and quadrature

| Data | Layout |
| --- | --- |
| Coordinates | `(dimension, numberOfPoints)` |
| Basis values | `(numberOfPoints, numberOfModes)` |
| Element basis gradients | `(numberOfPoints, numberOfModes, 1)` |
| Monomial degrees | `(numberOfMonomials, 1)` |
| Quadrature | Coordinate rows followed by one weight row; points in columns |

For degree `p`, there are `p+1` nodal DOFs and `p-1` interior DOFs for `p>=1`.
`EquidistancePoint_Line` uses endpoint first ordering. `InterpolationPoint_Line`
requires a layout: `VEFC` places vertices before interior points and
`INCREASING` requests increasing coordinates.

`BasisEvalAll_Line` and its derivatives support a polynomial family selector.
`OrthogonalBasis_Line` wraps the corresponding family on a reference interval.
`HeirarchicalBasis_Line` uses scaled integrated Legendre modes; its API retains
the historical spelling **Heirarchical**. Its first two modes are endpoint
functions, followed by interior modes. Oriented overloads apply signs to the
interior modes.

Lagrange coefficient columns represent cardinal basis functions. Coefficient
caches must match the nodes, degree and selected basis. Set `firstCall=.TRUE.`
when constructing a cache and explicitly pass `.FALSE.` to reuse it. In these
Line routines the flag is input; do not assume it is updated by the routine.
Keep evaluation points separate from the INOUT interpolation node array.

For quadrature, `order` means requested polynomial exactness degree. The
`nips=[n]` overload requests an explicit point count. Weights include the
selected family weight and, when vertex coordinates are supplied, the geometric
Jacobian. Keep coordinate and weight columns paired when changing layouts.

## Current implementation limits

- `Line_Quality` and `Quality_Line` return a placeholder zero for every measure.
- Hierarchical Laplacians reach empty Lobatto Laplacian implementations. Their
  examples deliberately use finite differences of the working gradient API.
- Monomial Laplacians work; selecting a named polynomial family can reach an
  unimplemented Polynomial backend. The examples use `BIUNIT`; second derivative
  scaling on `UNIT` in the element wrappers needs review.
- Low level basis value/derivative dispatch has BIUNIT assertions in debug
  builds. Use the documented reference wrappers when mapping from another domain.
- Some geometry interfaces retain fixed `kind=8` declarations. The examples use
  the project's `REAL(DFP)` configuration; they do not test all precision modes.

These documentation changes do not repair numerical implementations. Examples
that record placeholder behavior or show workarounds say so explicitly.

## Generate and validate

The shared [FORMAL project](../../../doc/formal.md) includes Line and Triangle.
Run from the project root:

```sh
formal generate --project doc/formal.md --mirror-sources
```

The generated pages are in
[docs/api/src/modules/Line/src](../../../docs/api/src/modules/Line/src/Line_Method.md).

Building, installing and running examples require explicit permission under
`AGENTS.md`. Once authorized, use the current installed library:

```sh
easifem install base --no-download
cd src/modules/Line/examples
easifem run segment_point_near_2d_test_1.F90
```
