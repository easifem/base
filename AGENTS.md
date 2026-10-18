# AGENTS.md - easifemBase Development Guide

This guide is for AI coding agents working on the easifemBase library.

## Project Overview

easifemBase is a Fortran library for finite element methods, part of the EASIFEM
(Expandable And Scalable Infrastructure for Finite Element Methods) framework.

- Language: Fortran 2018 (Modern Fortran)
- Build System: CMake 3.28+ with Ninja
- Compiler: GCC gfortran >=9.0 (tested with 15.2.0)
- Style: Multiple dispatch approach (not OOP except for string_class)

## Directory Structure

```
src/
├── modules/          # Module headers and interfaces
│   ├── BaseType/     # User-defined data types (BaseType.F90)
│   ├── BaseMethods/  # Methods for BaseType
│   └── <Module>/     # Each module has its own directory
└── submodules/       # Implementation in submodules
    └── <Module>/     # Implementations for corresponding module
```

**Key Pattern**: Module interfaces in `modules/`, implementations in `submodules/`

- Example: `modules/QuadraturePoint/src/QuadraturePoint_Method.F90` contains interfaces
- Example: `submodules/QuadraturePoint/src/QuadraturePoint_Method@ConstructorMethods.F90` contains implementations

## Build Commands

### Configure and Build

Always use easifem cli to build the project. For example

```bash
easifem dev base
```

```bash
# Using Python build script (recommended)
python3 build.py

# Or manually with CMake
cmake -G "Ninja" \
  -D CMAKE_BUILD_TYPE:STRING=Debug \
  -D CMAKE_INSTALL_PREFIX:PATH=${EASIFEM_BASE} \
  -D BUILD_SHARED_LIBS:BOOL=ON \
  -S ./ -B ${HOME}/temp/easifem/base/build

# Build
cmake --build ${HOME}/temp/easifem/base/build
```

### Build Types

- Debug: `-D CMAKE_BUILD_TYPE:STRING=Debug`
- Release: `-D CMAKE_BUILD_TYPE:STRING=Release`

### Install

```bash
cmake --install ${HOME}/temp/easifem/base/build
```

### Running Tests

There is no automated test suite setup. Tests are in:

- `src/modules/Test/src/examples/test_examples.F90`

To test changes, rebuild and verify with client code.

## Code Style Guidelines

### File Headers

Every library source file in `src/` must start with the GPL v3 license
header. Exception: test and example programs in
`src/modules/<Module>/examples/` must NOT contain the GPL header (see
"Examples and tests"). The header for library files is:

```fortran
! This program is a part of EASIFEM library
! Copyright (C) 2020-2021 Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>
```

### Naming Conventions

- **Types**: End with underscore: `CSRSparsity_`, `QuadraturePoint_`
- **Modules**: Match filename: `QuadraturePoint_Method` in `QuadraturePoint_Method.F90`
- **Submodules**: Format: `ParentModule@Category` e.g., `QuadraturePoint_Method@ConstructorMethods`
- **Variables**: camelCase: `ncol`, `nrow`, `isInitiated`
- **Constants**: ALL_CAPS from GlobalData: `I4B`, `DFP`, `LGT`

### Module Structure

```fortran
MODULE ModuleName_Method
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: TypeName_
USE String_Class, ONLY: String
IMPLICIT NONE
PRIVATE

! Public interface
PUBLIC :: MethodName

! Interface definitions
INTERFACE MethodName
  MODULE SUBROUTINE specific_impl(obj, param)
    TYPE(TypeName_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: param
  END SUBROUTINE specific_impl
END INTERFACE MethodName

END MODULE ModuleName_Method
```

### Submodule Structure

```fortran
SUBMODULE(ParentModule_Method) CategoryMethods
USE BaseMethod
USE ErrorHandling, ONLY: ErrorMsg
USE GlobalData, ONLY: stderr
IMPLICIT NONE

CONTAINS

MODULE PROCEDURE specific_impl
! Implementation here
END PROCEDURE specific_impl

END SUBMODULE CategoryMethods
```

### Import Rules

1. Always use `ONLY` clause in USE statements
2. Import order: GlobalData → BaseType → specific modules
3. Common imports:
   - `USE GlobalData, ONLY: DFP, I4B, LGT` (types)
   - `USE BaseType, ONLY: TypeName_` (user-defined types)
   - `USE String_Class, ONLY: String` (strings)

When we import two or more items from a module, then for each item imported from the module we should write it in its own line.
For example, If are importing two items foo and bar from XXX module then instead of using

```fortran
USE XXX, ONLY: foo, bar
```

We should write it in two lines as shown below:

```fortran
USE XXX, ONLY: foo
USE XXX, ONLY: bar
```

### Types

- **Real**: `REAL(DFP)` - Double precision floating point
- **Integer**: `INTEGER(I4B)` - 32-bit integer
- **Logical**: `LOGICAL(LGT)` - Logical type
- Always specify kind parameters from GlobalData

### Intent Keywords

Always specify intent for subroutine/function parameters:

- `INTENT(IN)` - Input only
- `INTENT(OUT)` - Output only
- `INTENT(INOUT)` - Modified

### Line Length

Maximum 78 characters (enforced by fortitude.toml)

### Implicit None

Always declare `IMPLICIT NONE` after module/submodule declaration

### Comments and FORD docblock style

- Use `!>` only for the *first line* of a FORD-style documentation block (the block header).
- The first line of a block must be `!> author: ...`. Do not add title lines such as `!> Module: Name`, `!> Subroutine: name` or `!> Function: name`; the name is already given by the declaration that follows the block.
- Use a plain `!` for all subsequent lines (continuation lines) in the same docblock.
- Use `!` for ordinary inline comments and short notes.
- Prefer LaTeX math for formulas in docblocks. Use display math `$$ ... $$` for full formulas.
- Keep docblock lines wrapped to the project's 78-character line length when possible.
- Document public interfaces and module-level summaries with a FORD-style block that includes author, date, summary, arguments, returns, a Formula section, and an Examples section.

Example FORD docblock (preferred style):

```fortran
!> author: Your Name, Ph.D.
! date: 2024-01-01
! summary: Short summary of the module.
!
! Description:
!   Longer description here. Wrap lines to <=78 characters.

MODULE SomeModule
...

!> author: Your Name
! date: 2024-01-01
! summary: Short summary for the function.
!
! Arguments:
!   x : REAL(DFP) - input value
!
! Returns:
!   y : REAL(DFP) - result
!
! Formula:
!   $$\mathrm{y} = \sum_{i} x_i\, w_i$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE SomeModule, ONLY: SomeFunction
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP) :: x(3), y
!
! x = [1.0_DFP, 2.0_DFP, 3.0_DFP]
! y = SomeFunction(x)
! CALL OK(y .GT. 0.0_DFP, "SomeFunction is positive")
!
! END PROGRAM main
!```

INTERFACE SomeFunction
  ...
END INTERFACE SomeFunction
```

Notes for AI agents and maintainers:

- When generating FORD docblocks, emit `!>` only on the first line of the block. Do not use `!>` on continuation lines.
- Use `!` continuation lines for all content after the first `!>` line so the files remain consistent with existing project examples (e.g., `ApproxUtility.F90`).
- Prefer clear, short lines and reflow text to avoid exceeding 78 characters.
- Use LaTeX only inside the Formula section; avoid complex LaTeX packages or commands that the documentation renderer may not support.
- Keep docblocks factual: include who authored the change, the date, a concise summary, argument/return shapes (using `SIZE(...)` where relevant), and a simple LaTeX formula for the mathematical relation.

#### Examples section in docblocks

- Every FORD docblock for a public routine must contain one example. Name the section `Examples` (not `Example`), written as `! Examples` on its own line.
- The example is a complete, runnable program, not a short snippet. It starts with `PROGRAM main` and ends with `END PROGRAM main`, and contains all `USE ..., ONLY:` lines, `IMPLICIT NONE`, declarations, the calls, and `CALL OK(...)` checks (from `Test_Method`) so that the result can be verified.
- Put the program inside a fenced code block. The fence is written as `` !```fortran `` and `` !``` `` on their own comment lines. All lines in the block use a plain `!` followed by one space. Do not use `!>`.
- The program is normally the same as the matching test in `src/modules/<Module>/examples/`, without the GPL header and without the `!> Test:` docblock.
- If a helper procedure is needed, put it after `CONTAINS` inside the example program.
- Keep the lines within 78 characters.
- Use only one `Examples` section per docblock. Do not keep a separate short `Example:` section next to it.

### Examples and tests

- Tests are standalone programs named `<Name>_test_<N>.F90` in `src/modules/<Module>/examples/`.
- For routines that are overloaded over several kinds, do not write tests for all the kinds.
  - For reals (`Real32`, `Real64`) use only `REAL(DFP)`.
  - For integers (`Int8`, `Int16`, `Int32`, `Int64`) use only `INTEGER(I4B)`.
- The same applies to the programs in the `Examples` section of the docblocks.
- Do not add the GPL license header to test programs. A test file starts directly with its `!> author:` docblock (or with `PROGRAM main` if there is none).

### Error Handling

Use ErrorHandling module for errors:

```fortran
USE ErrorHandling, ONLY: ErrorMsg
USE GlobalData, ONLY: stderr
```

## Common Patterns

### Creating a New Type

1. Define type in `src/modules/BaseType/src/BaseType.F90`
2. Create directory `src/modules/TypeName/`
3. Create `TypeName_Method.F90` with interfaces
4. Create directory `src/submodules/TypeName/`
5. Create implementation files: `TypeName_Method@CategoryMethods.F90`

### Categories of Methods

- `@ConstructorMethods` - Creation, initialization, copying
- `@IOMethods` - Display, read, write operations
- `@GetMethods` - Getter functions
- `@SetMethods` - Setter subroutines

## Compiler Flags

### Common Flags (gfortran)

- `-ffree-form` - Free-form Fortran
- `-ffree-line-length-none` - No line length limit
- `-std=f2018` - Fortran 2018 standard
- `-fimplicit-none` - Require explicit declarations

### Debug Flags

- `-fbounds-check` - Array bounds checking
- `-g` - Debug symbols
- `-fbacktrace` - Backtrace on error
- `-Wall -Wextra` - All warnings
- `-Wimplicit-interface` - Warn about implicit interfaces

### Release Flags

- `-O3` - Optimization level 3

## Environment Variables

- `EASIFEM_BASE` - Installation prefix
- `EASIFEM_BUILD_DIR` - Build directory (default: `$HOME/temp`)
- `EASIFEM_EXTPKGS` - External packages location

## Dependencies

External libraries (configured via CMAKE):

- OpenBLAS, LAPACK95, BLAS95
- SuperLU, LIS (linear solvers)
- ARPACK (eigensolvers)
- METIS (partitioning)
- HDF5 (I/O)
- PLPLOT (plotting)
- FFTW (FFT)

## Important Notes

1. **No Tests Per Se**: There's no `make test` or CTest infrastructure. Validate changes manually.

2. **Linting**: Use `fortitude` linter (config in `fortitude.toml`):
   - Checks: C, E, S, MOD, OB
   - File extensions: .f90, .F90
   - Line length: 78

3. **Documentation**: FORD (Fortran Documentation) - see `FORDsetup.md`

4. **Preprocessor**: Uses CMake preprocessor definitions:
   - `-DUSE_CMAKE`
   - `-DUSE_Real64` / `-DUSE_Real32`
   - `-DUSE_Int32` / `-DUSE_Int64`
   - `-D<OS>_SYSTEM` (Darwin, Linux, etc.)
   - `-DDEBUG_VER` (debug builds)

5. **Version**: Current version 24.10.3 (in CMakeLists.txt)

6. **License**: GPL v3 - include the header in new library source files (not in tests/examples)

## When Making Changes

1. Read existing code in the same module for patterns
2. Follow the module/submodule split architecture
3. Use `ONLY` clauses in all USE statements
4. Add proper GPL header to new library source files (not to tests/examples)
5. Maintain 78 character line length
6. Use MODULE PROCEDURE in submodules
7. Rebuild with `python3 build.py` to verify
8. Check fortitude linting if available
