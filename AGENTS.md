# AGENTS.md - easifemBase

Core layer of EASIFEM: data types (`BaseType`) and their methods, written
in a multiple-dispatch style (no OOP, except `String_Class`). Fortran
2018, CMake 3.28+ with Ninja, gfortran >= 9, GPL-3.0. easifemClasses and
the kernels depend on it.

Conventions, build rules, docblock and test rules are in the
`easifem-dev` Claude plugin (skill `easifem-conventions`). Agents without
the plugin: read
`https://github.com/easifem/claude-plugins/tree/main/easifem-dev/skills`.

Before searching for code, read `architecture.md`.

## Commands

- Build: `easifem dev base`
- Install: `easifem install base --no-download`
- Lint: `fortitude check --no-fix <changed files>`
- Docs: `formal generate --project doc/formal.md`
  (add the module to `src_dir` in `doc/formal.md` first)

## Paths

- All types: `src/modules/BaseType/src/BaseType.F90`
- Interface: `src/modules/<Name>/src/<Name>_Method.F90`
- Implementation: `src/submodules/<Name>/src/<Name>_Method@<Category>.F90`
- Tests: `src/modules/<Name>/examples/<MethodName>_test_<N>.F90`
- Errors: `ErrorHandling` (`ErrorMsg`), output unit `stderr` from
  `GlobalData`
- Docs output: `docs/api/`; `docs/pages/` are old website docs (read only)

## Build environment

- Variables: `EASIFEM_BASE` (install prefix), `EASIFEM_BUILD_DIR`
  (default `$HOME/temp`), `EASIFEM_EXTPKGS` (external packages).
- Preprocessor: `USE_CMAKE`, `USE_Real64`/`USE_Real32`,
  `USE_Int32`/`USE_Int64`, `<OS>_SYSTEM`, `DEBUG_VER` (Debug only).
- External libraries: OpenBLAS, LAPACK95, BLAS95, SuperLU, LIS, ARPACK,
  METIS, HDF5, PLPLOT, FFTW.
