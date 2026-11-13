# easifemBase architecture

This is a map of the repository structure. It does not document individual
routines. For coding conventions, see the `easifem-dev` plugin
(skill `easifem-conventions`); for repo facts, `AGENTS.md`.

## Top-level layout

| Path | Purpose |
|---|---|
| `CMakeLists.txt` | Defines the single library target `easifemBase` (v26.4.0), sets flags and `-D` macros, then includes `src/modules` and then `src/submodules` |
| `cmake/addX.cmake` | One file per external package: an option, find/link, and a compile macro |
| `src/modules/` | Module interfaces, one directory per topic |
| `src/submodules/` | Implementations, with the same directory names |
| `doc/formal.md` | Project file for the `formal` doc generator (`src_dir` list) |
| `docs/` | Generated API docs (`api/`) and hand-written pages. Gitignored |
| `ford.md` | Older FORD config (Utility only) |
| `python/`, `setup/` | Legacy build drivers and per-OS environment setup |
| `fortitude.toml` | Linter config: C,S rules, 78-column lines |

## Directory and file naming

Each `src/modules/<Dir>/` has `CMakeLists.txt` and `src/`. Some also have
`examples/` and a README.md. `src/submodules/<Dir>/` mirrors the same layout.

- **Interface module:** `src/modules/<Dir>/src/<Name>_Method.F90` defines the
  module `<Name>_Method`. Variants:
  - `<Name>Utility.F90`, used by Utility, Polynomial and the
    `*InterpolationUtility` modules.
  - Plain names such as `GlobalData`, `BaseType`, `ErrorHandling`.
- **Split modules plus umbrella:** large topics are split into
  `<Name>_<Category>Methods.F90` modules, and `<Name>_Method` USEs them all.
  Examples: CSRMatrix, DOF, RealVector, IntVector, FEVariable,
  ElemshapeData, Lapack.
- **Submodule:** `src/submodules/<Dir>/src/<ParentModule>@<Category>.F90`,
  for example `QuadraturePoint_Method@ConstructorMethods.F90`. Utility uses
  `<X>Utility@Methods.F90`, one per utility module.
- **Textual includes:** `.inc` files, and also `.F90` files inside
  subfolders (for example `submodules/Utility/src/Reallocate/`). These are
  `#include`d. They are not compiled on their own.
- **Tests:** `src/modules/<Dir>/examples/<Name>_test_<N>.F90`. A
  `<Name>__test_<N>.F90` name is the test for a routine whose name ends in
  `_`. Test files are standalone programs with no GPL header.
- **C sources** exist only in `System` (`easifemSystem` library) and
  `TriangleInterface` (`easifemTriangle`, Shewchuk's Triangle).

## Major modules (one line each)

**Foundation**
- `GlobalData`: kinds (`I4B`, `DFP`, `LGT`, …), stdout/stderr and enum
  constants. This is the leaf module.
- `PENF`, `FACE`, `BeFoR64`: vendored kind/format, ANSI colour and base64
  code.
- `String` (`String_Class`, `String_Method`): the only OOP type, derived
  from StringiFor.
- `Display` (`Display_Method`): generic Display/ToString built on the
  vendored dispmodule.
- `ErrorHandling`: `Errormsg` and related reporting routines.
- `BaseType`: every derived type, in one file.
- `Test` (`Test_Method`): TAP-style `OK`/`IS` used by the examples.
- `System`, `CInterface`, `OpenMP`, `Hashing`: OS, C interop, OpenMP and
  hashing helpers.

**Utilities and containers**
- `Utility`: umbrella over about 38 `*Utility` modules (sort, append,
  reallocate, products, mapping, …).
- `IntVector`, `RealVector`, `Vector3D`, `RealMatrix`, `IndexValue`,
  `KeyValue`: basic containers.
- `Rank2Tensor`, `VoigtRank2Tensor`: tensors.
- `MultiIndices`, `BoundingBox`, `Kdtree2`, `MdEncode`, `ConvergenceOpt`:
  multi-indices, bounding boxes, kd-tree, Markdown output and convergence
  options.

**Geometry and approximation**
- `Point`, `Line`, `Triangle`, `Quadrangle`, `Tetrahedron`, `Hexahedron`,
  `Prism`, `Pyramid`: each has `Reference<Elem>_Method` and
  `<Elem>InterpolationUtility`.
- `Geometry`: the `Geometry_Method` umbrella, plus `ReferenceElement_Method`
  and `Plane_Method`.
- `Polynomial`: 1D polynomial families. Its `PolynomialUtility` umbrella
  also re-exports every element `InterpolationUtility`.
- `BaseInterpolation`, `BaseContinuity`: interpolation and continuity type
  helpers.
- `QuadraturePoint`, `ElemshapeData`, `FEVariable`, `Projection`:
  quadrature, shape-function data, FE variables and projections.

**FE assembly and linear algebra**
- `MassMatrix`, `StiffnessMatrix`, `DiffusionMatrix`, `ConvectiveMatrix`,
  `ElasticNitscheMatrix`, `FacetMatrix` and their `ST*` (space-time)
  versions: element matrices. `FEMatrix` is their umbrella.
- `ForceVector`, `STForceVector`: element vectors. `FEVector` is their
  umbrella.
- `DOF`, `CSRSparsity`, `CSRMatrix`: degree-of-freedom maps and sparse
  storage.
- `Lapack`, `BLAS95`, `ARPACK`, `FFTW`: dense linear algebra, eigensolvers
  and FFT.
- `SuperLUInterface`, `MetisInterface`, `LuaInterface`, `RaylibInterface`,
  `TriangleInterface`, `LISInterface`: optional bindings to external
  libraries.

**Aggregators**
- `BaseMethod`: re-exports all method modules.
- `easifemBase`: `USE BaseType` plus `USE BaseMethod`. This is the public
  entry point for downstream code.

## Dependency direction

```text
GlobalData ─┬─> PENF/FACE/BeFoR64 ─> String_Class
            ├─> Display_Method ─> ErrorHandling
            └─> BaseType (GlobalData, String, [SuperLU])
                   │
                   v
     *_Method / *Utility interface modules (ONLY imports)
                   │
                   v
     topic umbrellas (Utility, PolynomialUtility, Geometry_Method,
                      CSRMatrix_Method, FEMatrix_Method, ...)
                   │
                   v
              BaseMethod ─> easifemBase ─> downstream (easifemClasses)

     submodules ── may USE BaseMethod (many do) or targeted ONLY lists
```

- Interface modules never USE `BaseMethod`. Submodules can, because a
  submodule is compiled after every module. This is how the code avoids
  module cycles.
- `BaseMethod` does not re-export Kdtree2, TriangleInterface,
  ConvergenceOptUtility, Projection, Raylib or LIS. USE these directly.
- CMake includes every module directory before any submodule directory.
  The order inside each list is not a dependency order: Ninja's Fortran
  dependency scanning works out the real order.

## Where to look for…

| Looking for | Go to |
|---|---|
| A derived type definition | `src/modules/BaseType/src/BaseType.F90` |
| Kinds, constants, element-name enums | `src/modules/GlobalData/src/GlobalData.F90` (+ `ElementNames.txt`) |
| A routine's interface | `src/modules/<Dir>/src/<Name>_Method.F90` |
| Its implementation | `src/submodules/<Dir>/src/<Name>_Method@*.F90` |
| Kind-generic bodies | `#include`d fragments in `submodules/<Dir>/src/<Sub>/` or `*.inc` |
| Reference element / basis functions | `src/modules/<Elem>/` and `src/modules/Polynomial/` |
| Element matrices/vectors | `src/modules/*Matrix/`, `ForceVector/`, `STForceVector/` |
| Sparse matrices | `src/modules/CSRSparsity/`, `src/modules/CSRMatrix/` |
| Example/test programs | `src/modules/<Dir>/examples/` |
| External package wiring | `cmake/add<Pkg>.cmake`, root `CMakeLists.txt` |
| Compile-time sizes (`MAX_*`, `REFELEM_MAX_*`) | root `CMakeLists.txt` cache variables |
| What gets documented | `src_dir` in `doc/formal.md` |

## Gotchas

- **No globbing.** Every source file must be listed by hand in its
  directory's `CMakeLists.txt`, and a new directory must be added to
  `src/{modules,submodules}/CMakeLists.txt`. Several unregistered files
  already exist and are never compiled (for example
  `ElemshapeData_DG*Methods.F90` and
  `GE_Lapack_Method@*Methods.F90`).
- **`.F90` include fragments.** Files inside include subfolders look like
  sources, but they are preprocessor templates that run between
  `#define`/`#undef` lines. Never add them to CMake.
- **BaseType is a single file.** Changing any type there recompiles nearly
  the whole library.
- **Very large files** (more than 5k lines): the `*InterpolationUtility`
  files for Line, Quadrangle and Tetrahedron, `ProductUtility`,
  `String_Class`.
- **Preprocessor everywhere.** These macros change the code that is
  compiled:
  - kinds: `USE_Real64`, `USE_Int32`, …
  - debug checks: `DEBUG_VER`, used at about 180 sites
  - optional packages: `USE_SuperLU`, `USE_METIS`, `USE_LAPACK95`, …
- **Dead or legacy code:**
  - `*-old/` folders and `submodules/{ST_Tau_SUPG_RGN,STFextVector,
    STFintVector,STStiffnessMatrix,IterationData,Vector}` are not built.
  - `submodules/FEMatrix` has no CMakeLists.
  - `modules/Macro` is unused.
  - `LISInterface`'s CMakeLists is commented out.
- **Odd paths.** `src/submodules/Rank2Tensor/src/old data/` has a space in
  its name. `Vector3D_Method@Misc.F90` sits outside `src/`. The shared
  `submodules/include/errors.F90` is included through relative paths.
- **Naming drift:** "Method" vs "Methods" suffixes, `_test_` vs `__test_`,
  and `Elemshapedata` vs `ElemshapeData` casing. The directory is
  `BaseMethod`, even though some docs say `BaseMethods`.
- **No CTest.** Examples are run one at a time with
  `easifem run X_test_N.F90` and checked with `OK` from `Test_Method`.
- **Build.** Use the `easifem` CLI (`easifem dev base`,
  `easifem install base --no-download`). In-source builds are a fatal
  error. Environment variables: `EASIFEM_BUILD_DIR`, `EASIFEM_BASE`,
  `EASIFEM_EXTPKGS`.
- **Line length.** gfortran runs with `-ffree-line-length-none`, so the
  78-column limit is enforced only by fortitude.
- **Dropbox.** The repo lives in Dropbox, so sync may touch build
  artefacts. Keep the build directory outside the repo.
