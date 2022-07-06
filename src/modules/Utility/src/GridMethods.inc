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

PUBLIC :: ExpMesh
PUBLIC :: Linspace
PUBLIC :: MeshGrid
PUBLIC :: arange


!----------------------------------------------------------------------------
!                                                  ExpMesh@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
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

!> authors: Vikas Sharma, Ph. D.
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

!> authors: Vikas Sharma, Ph. D.
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

!> authors: Vikas Sharma, Ph. D.
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
!                                                  MeshGrid@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
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

!----------------------------------------------------------------------------
!                                                   arange@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         3 March 2021
! summary: Returns a vector of reals given `start`,  `end`,  and `increment`
! values.

INTERFACE
  MODULE PURE FUNCTION arange_Real64(istart, iend, increment) RESULT(Ans)
    REAL(Real64), INTENT(IN) :: istart
    !! Start value of the array
    REAL(Real64), INTENT(IN) :: iend
    !! End value of the array
    REAL(Real64), INTENT(IN), OPTIONAL :: increment
    !! Array increment
    REAL(Real64), DIMENSION(:), ALLOCATABLE :: Ans
  END FUNCTION arange_Real64
END INTERFACE

INTERFACE arange
  MODULE PROCEDURE arange_Real64
END INTERFACE arange

!----------------------------------------------------------------------------
!                                                   arange@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         3 March 2021
! summary: Returns a vector of reals given `start`,  `end`,  and `increment`
! values.

INTERFACE
  MODULE PURE FUNCTION arange_Real32(istart, iend, increment) RESULT(Ans)
    REAL(Real32), INTENT(IN) :: istart
    !! Start value of the array
    REAL(Real32), INTENT(IN) :: iend
    !! End value of the array
    REAL(Real32), INTENT(IN), OPTIONAL :: increment
    !! Array increment
    REAL(Real32), DIMENSION(:), ALLOCATABLE :: Ans
  END FUNCTION arange_Real32
END INTERFACE

INTERFACE arange
  MODULE PROCEDURE arange_Real32
END INTERFACE arange

!----------------------------------------------------------------------------
!                                                    arangeFunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         3 March 2021
! summary: Returns a vector of integer
!
!# Introduction
! Returns an array of integers given `istart`,  `iend`,  and
! `increment` values. Default value of increment is 1
! This function belongs to the generic function [[Utility:arange]]
!
!### Usage
!
!```fortran
!        arange(1,10,1)
! arange(1,10,2)
!```

INTERFACE
  MODULE PURE FUNCTION arange_int(istart, iend, increment) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN), OPTIONAL :: increment
    INTEGER(I4B), DIMENSION(:), ALLOCATABLE :: Ans
  END FUNCTION
END INTERFACE

INTERFACE arange
  MODULE PROCEDURE arange_int
END INTERFACE arange
