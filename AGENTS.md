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
Every source file must start with the GPL v3 license header:
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

### Comments
- Use `!>` for author/date/summary documentation
- Use `!` for inline comments
- Document interfaces with author, date, and summary
```fortran
!> author: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: This routine does X
```

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

6. **License**: GPL v3 - always include header in new files

## When Making Changes

1. Read existing code in the same module for patterns
2. Follow the module/submodule split architecture
3. Use `ONLY` clauses in all USE statements
4. Add proper GPL header to new files
5. Maintain 78 character line length
6. Use MODULE PROCEDURE in submodules
7. Rebuild with `python3 build.py` to verify
8. Check fortitude linting if available
