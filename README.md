# easifemBase

**The foundation of [EASIFEM](https://www.easifem.com): kinds, containers,
geometry, interpolation, quadrature, sparse matrices and finite element
kernels — written in fast, modern Fortran.**

[![License: GPL-3.0](https://img.shields.io/badge/license-GPL--3.0-blue.svg)](LICENSE.gpl3.md)
[![Fortran 2018](https://img.shields.io/badge/Fortran-2018-734f96.svg)](https://fortran-lang.org/)
[![Version](https://img.shields.io/badge/version-26.4.0-blue.svg)](CMakeLists.txt)
[![Part of EASIFEM](https://img.shields.io/badge/part%20of-EASIFEM-orange.svg)](https://www.easifem.com)

`easifemBase` is the lowest layer of the EASIFEM platform. Everything
above it — [easifemClasses](https://github.com/easifem/classes), the
physics kernels
([elasticity](https://github.com/easifem/elasticityKernel),
[seepage](https://github.com/easifem/seepageKernel),
[diffusion](https://github.com/easifem/diffusionKernel)) and your own
applications — is built from the types and routines defined here.

The library uses a **multiple dispatch** style rather than class
hierarchies: every derived type is declared in `BaseType`, and methods
are generic interfaces in `*_Method` modules. `BaseMethod` re-exports
the standard set of those modules; a few specialist ones (`Kdtree2`,
`TriangleInterface`, `Projection`, `RaylibInterface`) are used directly.
`String_Class` is the only object-oriented type.

> EASIFEM stands for **E**xpandable **A**nd **S**calable
> **I**nfrastructure for **F**inite **E**lement **M**ethods.

## Where it fits

![Where easifemBase fits: easifemClasses, the physics kernels and
applications build on easifemBase, which links with external numerical
packages](figures/easifem-base-layers.svg)

`easifemBase` has no EASIFEM dependencies. It wraps BLAS, LAPACK95,
Sparsekit, SuperLU, METIS, HDF5, toml-f and other libraries, and exposes
one consistent Fortran API to the rest of the platform.

## Why use it

- **Batteries-included numerics.** BLAS and LAPACK95 wrappers, sparse
  CSR storage, eigensolvers through ARPACK, and FFTs through FFTW.
- **A finite element toolkit, not just a library of arrays.** Reference
  elements, Lagrange/Hermite/hierarchical interpolation, quadrature,
  shape-function data, FE variables, and element mass, stiffness,
  diffusion and force operators.
- **One data model for the whole stack.** All types live in `BaseType`,
  methods are generic, and `Display`, error handling, and kind
  parameters (`I4B`, `DFP`, `LGT`, …) behave the same everywhere.
- **Flexible builds without rewriting code.** Real32/real64 and
  int32/int64 variants, plus optional external packages, are selected
  with CMake options and preprocessor macros.

## What's inside

| Area                  | Representative types                                              | What it gives you                                 |
| --------------------- | ----------------------------------------------------------------- | ------------------------------------------------- |
| **Foundation**        | `GlobalData`, `BaseType`, `ErrorHandling`, `String_Class`         | Kinds, every derived type, errors and strings     |
| **Containers**        | `IntVector_`, `RealVector_`, `RealMatrix_`, `KeyValue_`           | Growable arrays, matrices and key-value maps      |
| **Geometry**          | `ReferenceLine_`, `ReferenceTriangle_`, `ReferenceHexahedron_`    | Reference elements and physical mappings          |
| **Approximation**     | `QuadraturePoint_`, `ElemshapeData_`, `FEVariable_`               | Quadrature rules, shape data and field values     |
| **Polynomials**       | `LagrangeInterpolation_`, `HermitInterpolation_`                  | Lagrange, Hermite and hierarchical bases          |
| **Element assembly**  | `MassMatrix`, `StiffnessMatrix`, `DiffusionMatrix`, `ForceVector` | Element matrices and vectors                      |
| **Sparse algebra**    | `DOF_`, `CSRSparsity_`, `CSRMatrix_`                              | DOF maps, sparsity patterns and CSR matrices      |
| **Dense LA and FFT**  | LAPACK95, ARPACK and FFTW interfaces                              | Dense solvers, eigensolvers and transforms        |
| **Utilities**         | `Utility` umbrella, `Kdtree2`, `MultiIndices`, `MdEncode`         | Sorting, appending, k-d trees and Markdown output |
| **Optional bindings** | `SuperLUInterface`, `MetisInterface`, `TriangleInterface`         | SuperLU, METIS, Triangle, LIS, Lua and Raylib     |

The tree contains 2,289 Fortran sources, 74 topic directories, and over
1,100 example programs that check their own results with `OK` and `IS`
from `Test_Method`.

## Using the library

The public entry point re-exports `BaseType` and the standard method
modules via `BaseMethod`:

```fortran
USE easifemBase
```

For tighter namespaces, use the two halves directly:

```fortran
USE BaseType      ! derived types and kind parameters
USE BaseMethod    ! re-exports the standard method modules
```

A new derived type is declared in `BaseType`, its interface goes into the
matching `*_Method` module, and that module is added to `BaseMethod` so
downstream code sees it through `USE easifemBase`.

## Quick start

### Requirements

| Requirement            | Notes                               |
| ---------------------- | ----------------------------------- |
| GFortran 9+            | GNU builds use `-std=f2018`         |
| CMake 3.28+            | Ninja is recommended                |
| BLAS and LAPACK        | OpenBLAS is recommended             |
| LAPACK95 and Sparsekit | Installed by the EASIFEM tooling    |
| HDF5 and toml-f        | Fortran components must be findable |

### Install with the EASIFEM CLI

```bash
# 1. Install the external packages once
easifem install extpkgs

# 2. Install the base library
easifem install base
```

If the EASIFEM directories are not configured yet, run `easifem setenv`
once first; see `easifem setenv --help`.

### Build with CMake

```bash
cmake -S . -B build -G Ninja -D CMAKE_BUILD_TYPE=Release
cmake --build build --parallel
cmake --install build --prefix /path/to/easifem
```

If CMake cannot find HDF5, toml-f, LAPACK95 or Sparsekit, add their
install prefixes to `CMAKE_PREFIX_PATH`.

### Use it from another project

```cmake
find_package(easifemBase REQUIRED)
target_link_libraries(my_app PRIVATE easifemBase::easifemBase)
```

### A first program

This builds an integer vector with appended values and prints it. The
same `Append` interface accepts a scalar, an array, or another vector.

```fortran
PROGRAM main
  USE easifemBase
  IMPLICIT NONE

  TYPE(IntVector_) :: v
  INTEGER(I4B) :: ii

  DO ii = 1, 5
    CALL Append(v, 10_I4B * ii)
  END DO

  PRINT *, "size = ", Size(v)
  CALL Display(v%val(1:Size(v)), "v = ")
END PROGRAM main
```

Adapted from `src/modules/IntVector/examples/Append_test_1.F90`.

## Developing the library

Build this checkout in debug mode, with bounds checks and nothing
installed:

```bash
easifem dev base
```

Run one module's examples. Most `examples/` directories have a
`runner.toml`; the programs assert their own results and double as tests.

```bash
cd src/modules/IntVector/examples
easifem test ./
```

Lint changed sources (78-column lines, C and S rules):

```bash
fortitude check --no-fix <changed files>
```

Generate the API reference with [FORD](https://forddocs.readthedocs.io/):

```bash
formal generate --project ford.md
```

## Repository layout

```text
src/modules/BaseType/src/BaseType.F90        every derived type, in one file
src/modules/<Topic>/src/<Name>_Method.F90    public interfaces
src/modules/<Topic>/examples/                runnable examples and tests
src/submodules/<Topic>/src/                  implementations
src/modules/BaseMethod/src/BaseMethod.F90    re-exports method modules
cmake/add<Pkg>.cmake                         optional external packages
```

Every source file is listed explicitly in its directory's
`CMakeLists.txt`; there is no globbing. See
[architecture.md](architecture.md) for the dependency order, the module
map and the known gotchas.

## Optional features

| Option         | Default | Adds                      |
| -------------- | ------- | ------------------------- |
| `USE_SUPERLU`  | ON      | Sparse direct solver      |
| `USE_METIS`    | ON      | Graph partitioning        |
| `USE_OPENMP`   | OFF     | Shared-memory parallelism |
| `USE_ARPACK`   | OFF     | Sparse eigensolvers       |
| `USE_FFTW`     | OFF     | Fast Fourier transforms   |
| `USE_LIS`      | OFF     | Iterative linear solvers  |
| `USE_PLPLOT`   | OFF     | Plotting                  |
| `USE_LUA`      | OFF     | Lua scripting             |
| `USE_RAYLIB`   | OFF     | Simple graphics           |
| `USE_GMSH_SDK` | OFF     | Gmsh mesh generation      |
| `USE_GTK`      | OFF     | GTK-4 graphical interface |

Enable an option only when the library is installed and can be found by
CMake, for example `-D USE_FFTW=ON`.

The numeric model is also selectable at build time:

- `USE_REAL64` (default ON) or `USE_REAL32`
- `USE_INT32` (default ON) or `USE_INT64`
- `USE_COLORDISP` (default ON) for ANSI-coloured terminal output

## Documentation and community

- **Website and guides:** <https://www.easifem.com>
- **API reference:** generate locally with `formal` (see above)
- **Cookbook:** <https://github.com/easifem/cookbook>
- **Issues and questions:** <https://github.com/easifem/base/issues>
- **Object-oriented layer:** <https://github.com/easifem/classes>

## Contributing

Contributions are welcome. Development happens on the `dev` branch, so
please base pull requests there.

- Read [architecture.md](architecture.md) before adding a type, and
  [AGENTS.md](AGENTS.md) for the repository conventions.
- Declare new types in `BaseType.F90`; keep the multiple-dispatch style.
- Add the interface to `src/modules/<Topic>/src/<Name>_Method.F90` and the
  implementation to a submodule in `src/submodules/<Topic>/src/`.
- Register every new file in the matching `CMakeLists.txt`.
- Add a `<Method>_test_<N>.F90` example; the examples are the tests.
- Run `fortitude check --no-fix` on the files you changed.

## License

easifemBase is free software under the GNU General Public License,
version 3 or later. See [LICENSE.gpl3.md](LICENSE.gpl3.md) and
[LICENSE](LICENSE).

---

**EASIFEM** — Expandable And Scalable Infrastructure for Finite Element
Methods. Written in modern Fortran; the foundation everything else is
built on.
