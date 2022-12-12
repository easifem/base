! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
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
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

MODULE GridPointUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: ExpMesh
PUBLIC :: Linspace
PUBLIC :: Logspace
PUBLIC :: MeshGrid

!----------------------------------------------------------------------------
!                                                  ExpMesh@FunctionalFortran
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Sept 2021
! summary: Exponential mesh

INTERFACE
  MODULE PURE FUNCTION ExpMesh_Real64(rmin, rmax, a, N) RESULT(Ans)
    REAL(Real64), INTENT(IN) :: rmin
    !! left end of 1D domain
    REAL(Real64), INTENT(IN) :: rmax
    !! right end of 1D domain
    REAL(Real64), INTENT(IN) :: a
    !! Ratio of largest to smallest element, a should be positive
    !! a = 1, then we get uniform mesh
    INTEGER(I4B), INTENT(IN) :: N
    !! Number of elements present in mesh
    REAL(Real64) :: ans(N + 1)
    !! Number of nodes in mesh
  END FUNCTION ExpMesh_Real64
END INTERFACE

INTERFACE ExpMesh
  MODULE PROCEDURE ExpMesh_Real64
END INTERFACE ExpMesh

!----------------------------------------------------------------------------
!                                                  ExpMesh@FunctionalFortran
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Sept 2021
! summary: Exponential mesh

INTERFACE
  MODULE PURE FUNCTION ExpMesh_Real32(rmin, rmax, a, N) RESULT(Ans)
    REAL(Real32), INTENT(IN) :: rmin
    !! left end of 1D domain
    REAL(Real32), INTENT(IN) :: rmax
    !! right end of 1D domain
    REAL(Real32), INTENT(IN) :: a
    !! Ratio of largest to smallest element, a should be positive
    !! a = 1, then we get uniform mesh
    INTEGER(I4B), INTENT(IN) :: N
    !! Number of elements present in mesh
    REAL(Real32) :: ans(N + 1)
    !! Number of nodes in mesh
  END FUNCTION ExpMesh_Real32
END INTERFACE

INTERFACE ExpMesh
  MODULE PROCEDURE ExpMesh_Real32
END INTERFACE ExpMesh

!----------------------------------------------------------------------------
!                                                  Linspace@FunctionalFortran
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Sept 2021
! summary: linspace

INTERFACE
  MODULE PURE FUNCTION Linspace_Real64(a, b, N) RESULT(Ans)
    REAL(Real64), INTENT(IN) :: a
    !! left end of 1D domain
    REAL(Real64), INTENT(IN) :: b
    !! right end of 1D domain
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: N
    !! Number of points including a and b
    REAL(Real64), ALLOCATABLE :: ans(:)
    !! Number of nodes in mesh
  END FUNCTION Linspace_Real64
END INTERFACE

INTERFACE Linspace
  MODULE PROCEDURE Linspace_Real64
END INTERFACE Linspace

!----------------------------------------------------------------------------
!                                                  Linspace@FunctionalFortran
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Sept 2021
! summary: Returns a linearly spaced vector
!
!# Introduction
! Returns a linearly spaced vector with n points in [a, b]
! if n is omitted, 100 points will be considered

INTERFACE
  MODULE PURE FUNCTION Linspace_Real32(a, b, N) RESULT(Ans)
    REAL(Real32), INTENT(IN) :: a
    !! left end of 1D domain
    REAL(Real32), INTENT(IN) :: b
    !! right end of 1D domain
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: N
    !! Number of points including a and b
    REAL(Real32), ALLOCATABLE :: ans(:)
    !! Number of nodes in mesh
  END FUNCTION Linspace_Real32
END INTERFACE

INTERFACE Linspace
  MODULE PROCEDURE Linspace_Real32
END INTERFACE Linspace

!----------------------------------------------------------------------------
!                                                  Logspace@FunctionalFortran
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Sept 2021
! summary: Logspace

INTERFACE
  MODULE PURE FUNCTION Logspace_Real64(a, b, N, endPoint, base) RESULT(Ans)
    REAL(Real64), INTENT(IN) :: a
    !! left end of 1D domain
    REAL(Real64), INTENT(IN) :: b
    !! right end of 1D domain
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: N
    !! Number of points including a and b
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: endPoint
    !! default is true, if true then include endpoint
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: base
    !! default is 10
    REAL(Real64), ALLOCATABLE :: ans(:)
    !! Number of nodes in mesh
  END FUNCTION Logspace_Real64
END INTERFACE

INTERFACE Logspace
  MODULE PROCEDURE Logspace_Real64
END INTERFACE Logspace

!----------------------------------------------------------------------------
!                                                  Logspace@FunctionalFortran
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Sept 2021
! summary: Logspace

INTERFACE
  MODULE PURE FUNCTION Logspace_Real32(a, b, N, endPoint, base) RESULT(Ans)
    REAL(Real32), INTENT(IN) :: a
    !! left end of 1D domain
    REAL(Real32), INTENT(IN) :: b
    !! right end of 1D domain
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: N
    !! Number of points including a and b
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: endPoint
    !! default is true, if true then include endpoint
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: base
    !! default is 10
    REAL(Real32), ALLOCATABLE :: ans(:)
    !! Number of nodes in mesh
  END FUNCTION Logspace_Real32
END INTERFACE

INTERFACE Logspace
  MODULE PROCEDURE Logspace_Real32
END INTERFACE Logspace

!----------------------------------------------------------------------------
!                                                  MeshGrid@FunctionalFortran
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 Sept 2021
! summary: meshgrid generate mesh grid over a rectangular domain
!
!# Introduction
!
! Meshgrid generate mesh grid over a rectangular domain of
! [xmin xmax, ymin, ymax]
! - xgv, ygv are grid vectors in form of full grid data
! - X and Y are matrix each of size [ny by nx] contains the grid data.
! - The coordinates of point (i,j) is [X(i,j), Y(i,j)]
!
!### Usage
!
!```fortran
! call meshgrid(X, Y, [0.,1.,2.,3.],[5.,6.,7.,8.])
!
!  X =
!  [0.0, 1.0, 2.0, 3.0,
!   0.0, 1.0, 2.0, 3.0,
!   0.0, 1.0, 2.0, 3.0,
!   0.0, 1.0, 2.0, 3.0]
!
! Y =
! [ 5.0, 5.0, 5.0, 5.0,
!   6.0, 6.0, 6.0, 6.0,
!   7.0, 7.0, 7.0, 7.0,
!   8.0, 8.0, 8.0, 8.0]
!```

INTERFACE
  MODULE PURE SUBROUTINE MeshGrid2D_Real64(x, y, xgv, ygv)
    REAL(Real64), ALLOCATABLE, INTENT(INOUT) :: x(:, :)
    REAL(Real64), ALLOCATABLE, INTENT(INOUT) :: y(:, :)
    REAL(Real64), INTENT(IN) :: xgv(:)
    REAL(Real64), INTENT(IN) :: ygv(:)
  END SUBROUTINE MeshGrid2D_Real64
END INTERFACE

INTERFACE MeshGrid
  MODULE PROCEDURE MeshGrid2D_Real64
END INTERFACE MeshGrid

!----------------------------------------------------------------------------
!                                                MeshGrid@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE MeshGrid2D_Real32(x, y, xgv, ygv)
    REAL(Real32), ALLOCATABLE, INTENT(INOUT) :: x(:, :)
    REAL(Real32), ALLOCATABLE, INTENT(INOUT) :: y(:, :)
    REAL(Real32), INTENT(IN) :: xgv(:)
    REAL(Real32), INTENT(IN) :: ygv(:)
  END SUBROUTINE MeshGrid2D_Real32
END INTERFACE

INTERFACE MeshGrid
  MODULE PROCEDURE MeshGrid2D_Real32
END INTERFACE MeshGrid

!----------------------------------------------------------------------------
!                                               MeshGrid@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE MeshGrid3D_Real64(x, y, z, xgv, ygv, zgv)
    REAL(Real64), ALLOCATABLE, INTENT(INOUT) :: x(:, :, :)
    REAL(Real64), ALLOCATABLE, INTENT(INOUT) :: y(:, :, :)
    REAL(Real64), ALLOCATABLE, INTENT(INOUT) :: z(:, :, :)
    REAL(Real64), INTENT(IN) :: xgv(:)
    REAL(Real64), INTENT(IN) :: ygv(:)
    REAL(Real64), INTENT(IN) :: zgv(:)
  END SUBROUTINE MeshGrid3D_Real64
END INTERFACE

INTERFACE MeshGrid
  MODULE PROCEDURE MeshGrid3D_Real64
END INTERFACE MeshGrid

!----------------------------------------------------------------------------
!                                               MeshGrid@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE MeshGrid3D_Real32(x, y, z, xgv, ygv, zgv)
    REAL(Real32), ALLOCATABLE, INTENT(INOUT) :: x(:, :, :)
    REAL(Real32), ALLOCATABLE, INTENT(INOUT) :: y(:, :, :)
    REAL(Real32), ALLOCATABLE, INTENT(INOUT) :: z(:, :, :)
    REAL(Real32), INTENT(IN) :: xgv(:)
    REAL(Real32), INTENT(IN) :: ygv(:)
    REAL(Real32), INTENT(IN) :: zgv(:)
  END SUBROUTINE MeshGrid3D_Real32
END INTERFACE

INTERFACE MeshGrid
  MODULE PROCEDURE MeshGrid3D_Real32
END INTERFACE MeshGrid

END MODULE GridPointUtility
