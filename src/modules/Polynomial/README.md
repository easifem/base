# Polynomial utilities

The twelve modules in this directory provide polynomial evaluation,
recurrences, quadrature, modal transforms, nodal interpolation and hierarchical
bases. Interfaces and examples are documented in the Fortran sources; numerical
implementations remain in `src/submodules/Polynomial/src`.

## Module guide

| Module | Purpose |
| --- | --- |
| [PolynomialUtility](src/PolynomialUtility.F90) | Umbrella reexports, including element interpolation modules outside this directory |
| [LegendrePolynomialUtility](src/LegendrePolynomialUtility.F90) | Legendre polynomials, unit weight |
| [Chebyshev1PolynomialUtility](src/Chebyshev1PolynomialUtility.F90) | First kind Chebyshev polynomials, weight `(1-x*x)**(-1/2)` |
| [JacobiPolynomialUtility](src/JacobiPolynomialUtility.F90) | Jacobi polynomials with parameters `alpha`, `beta` |
| [UltrasphericalPolynomialUtility](src/UltrasphericalPolynomialUtility.F90) | Gegenbauer polynomials with parameter `lambda` |
| [LobattoPolynomialUtility](src/LobattoPolynomialUtility.F90) | Scaled integrated Legendre modes, kernels, mass and stiffness matrices |
| [UnscaledLobattoPolynomialUtility](src/UnscaledLobattoPolynomialUtility.F90) | Unscaled integrated Legendre modes |
| [OrthogonalPolynomialUtility](src/OrthogonalPolynomialUtility.F90) | Clenshaw sums and family/element dispatch |
| [InterpolationUtility](src/InterpolationUtility.F90) | Monomial Vandermonde matrices, DOF counts and domain names |
| [LagrangePolynomialUtility](src/LagrangePolynomialUtility.F90) | Interpolation nodes, coefficients, values and gradients |
| [HierarchicalPolynomialUtility](src/HierarchicalPolynomialUtility.F90) | Entity DOFs and oriented hierarchical bases |
| [RecursiveNodesUtility](src/RecursiveNodesUtility.F90) | Recursive simplex interpolation nodes |

## Normalization and array conventions

Legendre, Jacobi, Chebyshev and Gegenbauer evaluators use their standard
normalizations, rather than unit norms or monic normalizations. Monic recurrence
coefficients are returned separately by `Get*RecurrenceCoeff`. The standard
three term coefficients are returned by `Get*RecurrenceCoeff2`.

For Jacobi polynomials the integration weight is

$$w(x)=(1-x)^\alpha(1+x)^\beta,\qquad \alpha,\beta>-1.$$

For Gegenbauer polynomials it is

$$w(x)=(1-x^2)^{\lambda-1/2}.$$

Use positive `lambda` with the documented examples. `lambda=1/2` recovers
Legendre normalization. Several formulas are singular at `lambda=0`; use the
Chebyshev module for first kind Chebyshev polynomials.

The scaled and unscaled interior Lobatto modes are

$$\phi_n(x)=\frac{P_n(x)-P_{n-2}(x)}{\sqrt{2(2n-1)}},\qquad
\psi_n(x)=\frac{P_n(x)-P_{n-2}(x)}{2n-1},\qquad n\ge2.$$

Both families start with endpoint functions `(1-x)/2` and `(1+x)/2`.
Interior modes vanish at the endpoints. Their mass matrices are not diagonal;
`LobattoTransform_` solves a mass matrix system.

| Quantity | Layout |
| --- | --- |
| One polynomial at vector `x` | `SIZE(x)` entries |
| All degrees through `n`, scalar `x` | `n+1` entries, degree zero first |
| All degrees through `n`, vector `x` | `(SIZE(x), n+1)`, point rows, degree columns |
| Element coordinates | `(dimension, numberOfPoints)`, points in columns |
| Element basis values | `(numberOfPoints, numberOfBasisFunctions)` |
| Element basis gradients | `(numberOfPoints, numberOfBasisFunctions, dimension)` |
| `LagrangeDegree` | `(numberOfMonomials, dimension)` |
| Monomial expansion matrix | Rows are increasing powers; columns are degrees |
| Modal and recurrence arrays | Dummy arguments commonly start at index zero |
| Lobatto kernels | Second index starts at zero |

Subroutines ending in `_` generally write into caller supplied storage and return
active dimensions (`tsize`, `nrow`, `ncol` or `dim1:dim3`). Allocate sufficient
storage; use only the active result region. Function wrappers allocate or size
their results. Ordinary Fortran assignment can change result lower bounds, so
explicitly declare zero based arrays when later indexing by degree.

## Quadrature point counts

| Interface | Meaning of `n` | Output point count |
| --- | --- | --- |
| `*GaussQuadrature` | Number of points | `n` |
| `*GaussRadauQuadrature` | Number of free points | `n+1` |
| `*GaussLobattoQuadrature` | Number of free points | `n+2` |
| `*Quadrature` | Total requested points | `n` |

The corresponding Jacobi matrices have the same number of diagonal entries and
one fewer off diagonal entry. Radau uses `a=-1` or `a=+1` to select the prescribed
endpoint. The high level dispatchers accept `Gauss`, `GaussRadauLeft`,
`GaussRadauRight` and `GaussLobatto` from `GlobalData`.

Weights include the polynomial family weight. Keep nodes and weights paired;
node ordering varies between families. Polynomial evaluation takes a degree,
which is not necessarily a quadrature routine's `n`.

`LobattoZeros(n)` and `UnscaledLobattoZeros(n)` return **n interpolation nodes**,
including both endpoints for `n>=2`. Their interior points are roots of a Jacobi
polynomial of degree `n-2` with parameters `(1,1)`. They do not return the zeros
of `LobattoEval(n,x)`. The `n=1` special case returns `+1`.

## Interpolation and series evaluation

For a monomial Vandermonde matrix `V`, Lagrange coefficients satisfy `V*C=I`.
Columns of `C` are nodal basis functions. `LagrangeEvalAll` and its gradient form
can cache coefficients: initialize `firstCall=.TRUE.` and pass a coefficient
buffer. Reuse that buffer only for the same nodes, order, domain and basis.
The node argument `xij` is `INTENT(INOUT)` and may be transformed by a backend;
keep a separate coordinate array for evaluation points.

`InterpolationPoint` requires a layout string. For example, `INCREASING` selects
increasing Line coordinates, while `VEFC` requests entity ordering where
supported. `EquidistancePoint` places vertices before interior nodes.

`Clenshaw(x,alpha,beta,c)` evaluates a monic recurrence series.
`Clenshaw(x,c)` and vector `ChebClenshaw(x,c)` instead use

$$s(x)=c_0/2+\sum_{j=1}^{n}c_jT_j(x).$$

`Chebyshev1EvalSum` uses the full constant coefficient. Transform input
`coeff` means sampled function values; transform outputs are modal coefficients.
Callback transform overloads evaluate an `iface_1DFunction` procedure pointer
on an interval `[x1,x2]` mapped from reference coordinates.

## Current implementation limits

These observations come from the current implementations, rather than the old
website examples. This documentation change does not repair numerical routines.

- The six family `*LaplacianEval`, `*LaplacianEvalAll` and
  `*LaplacianEvalAll_` APIs have empty implementation bodies or forward to them.
  Their results and output dimensions are undefined. Their examples demonstrate
  a central difference of the implemented gradient API instead of calling the
  empty routines. These are workaround examples, not passing Laplacian tests.
- `LaplacianEvalAllOrthopol` currently works for `poly%Monomial`; the other
  family branches reach those empty bodies. Element Laplacian dispatch only
  supports Line. Its examples use `BIUNIT`; the UNIT backend's second derivative
  scaling also needs review.
- Hierarchical value and gradient dispatch currently implements Line, Triangle
  and Quadrangle. DOF helpers cover more topologies. The function wrappers
  require all three orientation arguments, even when some are unused. In 2D,
  `faceOrder` describes boundary edges; in 3D, `edgeOrder` describes edges.
- The even `n` branch of `*DMatEvenOdd` does not assign every output entry.
  Examples use odd `n`, where the blocks are fully assigned.
- The two Lobatto `LeadingCoeff` implementations return the opposite endpoint
  signs for `n=0,1` from the signs in the evaluators. Examples use `n=2`.
- `Chebyshev1NormSQRRatio` returns one for every degree, whereas the mathematical
  ratio at degree zero is one half. Use the norm functions for that case.
- Recursive node builders have fixed internal work arrays. Use positive orders
  no larger than 99; coordinate mapping helpers in that module are private.

Some quadrature, root, projection and interpolation routines require the
configured LAPACK95/linear algebra backend. A declaration alone does not imply
that every optional backend is enabled in the installed library.

## Examples and generated reference

[The examples index](examples/README.md) lists every new standalone program.
Each contains `CALL OK(...)` checks and uses only `REAL(DFP)` and `INTEGER(I4B)`
for numerical kind coverage. The existing `UltrasphericalRows_test_1.F90`
regression is preserved.

Building, installing and running examples require explicit user permission
under the repository's `AGENTS.md`. Once authorized, install the current library
and run an example from its directory:

```sh
easifem install base --no-download
cd src/modules/Polynomial/examples
easifem run LegendreEval_test_1.F90
```

Generate the [Markdown API reference](../../../doc/api/polynomial/index.md)
without building the library:

```sh
python3 doc/generate_polynomial_docs.py
```

The dedicated project scans just Polynomial interfaces and uses the repository's
`!> author:` / plain `!` documentation markers. It leaves the existing Utility
documentation project unchanged. The generator also corrects FORMAL navigation
for this nested output path and distinguishes trailing underscore API anchors.
Old `$docs` and `$tests` materials were used
as references; examples here follow the current source interfaces.
