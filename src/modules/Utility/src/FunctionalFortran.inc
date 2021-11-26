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
  MODULE PROCEDURE ExpMesh_Real32, ExpMesh_Real64
END INTERFACE ExpMesh

PUBLIC :: ExpMesh

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
  MODULE PROCEDURE Linspace_Real32, Linspace_Real64
END INTERFACE Linspace

PUBLIC :: Linspace

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
  MODULE PROCEDURE MeshGrid2D_Real32, MeshGrid2D_Real64, &
    & MeshGrid3D_Real32, MeshGrid3D_Real64
END INTERFACE MeshGrid

PUBLIC :: MeshGrid

!----------------------------------------------------------------------------
!                                                   arange@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         3 March 2021
! summary: Returns a vector of reals given `start`,  `end`,  and `increment`
! values.

INTERFACE
  MODULE PURE FUNCTION arange_Real64(istart, iend, increment) result(Ans)
    REAL(Real64), INTENT(IN) :: istart
    !! Start value of the array
    REAL(Real64), INTENT(IN) :: iend
    !! End value of the array
    REAL(Real64), INTENT(IN), OPTIONAL :: increment
    !! Array increment
    REAL(Real64), DIMENSION(:), ALLOCATABLE :: Ans
  END FUNCTION arange_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                                   arange@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         3 March 2021
! summary: Returns a vector of reals given `start`,  `end`,  and `increment`
! values.

INTERFACE
  MODULE PURE FUNCTION arange_Real32(istart, iend, increment) result(Ans)
    REAL(Real32), INTENT(IN) :: istart
    !! Start value of the array
    REAL(Real32), INTENT(IN) :: iend
    !! End value of the array
    REAL(Real32), INTENT(IN), OPTIONAL :: increment
    !! Array increment
    REAL(Real32), DIMENSION(:), ALLOCATABLE :: Ans
  END FUNCTION arange_Real32
END INTERFACE

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
  MODULE PURE FUNCTION arange_int(istart, iend, increment) result(Ans)
    integer(i4b), intent(in) :: istart
    integer(i4b), intent(in) :: iend
    integer(i4b), intent(in), optional :: increment
    integer(i4b), dimension(:), allocatable :: Ans
  END FUNCTION
END INTERFACE

INTERFACE arange
  MODULE PROCEDURE arange_int, arange_real64, arange_Real32
END INTERFACE arange

PUBLIC arange

!----------------------------------------------------------------------------
!                                                     Head@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Returns the first element of array `x`.

INTERFACE
  MODULE PURE FUNCTION head_int(x) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: x(:)
    INTEGER(I4B) :: Ans
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Head@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Returns the first element of array `x`.

INTERFACE
  MODULE PURE FUNCTION head_real(x) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: Ans
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Head@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Returns the first element of array `x`.

INTERFACE
  MODULE PURE FUNCTION head_char(x) RESULT(Ans)
    CHARACTER(LEN=*), INTENT(IN) :: x
    CHARACTER(LEN=1) :: Ans
  END FUNCTION
END INTERFACE

INTERFACE HEAD
  MODULE PROCEDURE head_real, head_int, head_char
END INTERFACE HEAD

PUBLIC :: HEAD

!----------------------------------------------------------------------------
!                                                     Tail@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION tail_int(x) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: x(:)
    INTEGER(I4B) :: Ans(SIZE(x) - 1)
  END FUNCTION tail_int
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Tail@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION tail_real(x) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: Ans(SIZE(x) - 1)
  END FUNCTION tail_real
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Tail@FunctionalFortran
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION tail_char(x) RESULT(Ans)
    CHARACTER(LEN=*), INTENT(IN) :: x
    CHARACTER(LEN=LEN(x) - 1) :: Ans
  END FUNCTION tail_char
END INTERFACE

INTERFACE TAIL
  MODULE PROCEDURE tail_int, tail_real, tail_char
END INTERFACE TAIL

PUBLIC :: TAIL

!----------------------------------------------------------------------------
!                                                    SPLIT@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Returns the first half of the array `x` if `section == 1`
!
!# Introduction
!
! Returns the first half of the array `x` if `section == 1`, the second half
! of the array `x` if `section == 2`, and an empty array otherwise. If `size
! (x) == 1`,  `split(x, 1)`  returns and empty array,  and `split(x, 2)`
! returns `x(1)`.

INTERFACE
  MODULE PURE FUNCTION split_int(x, section) RESULT(Ans)
    INTEGER(I4B), DIMENSION(:), INTENT(IN) :: x !! Input array
    INTEGER(I4B), INTENT(IN) :: section !! Array section to return
    INTEGER(I4B), DIMENSION(:), ALLOCATABLE :: Ans
  END FUNCTION split_int
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SPLIT@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Returns the first half of the array `x` if `section == 1`,
!
!# Introduction
!
! Returns the first half of the array `x` if `section == 1`, the second half
! of the array `x` if `section == 2`, and an empty array otherwise. If `size
! (x) == 1`,  `split(x, 1)`  returns and empty array,  and `split(x, 2)`
! returns `x(1)`.

INTERFACE
  MODULE PURE FUNCTION split_real(x, section) RESULT(Ans)
    REAL(DFP), DIMENSION(:), INTENT(IN) :: x !! Input array
    INTEGER(I4B), INTENT(IN) :: section !! Array section to return
    REAL(DFP), DIMENSION(:), ALLOCATABLE :: Ans
  END FUNCTION split_real
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SPLIT@FunctionalFortran
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Returns the first half of the array `x` if `section == 1`,
!
!# Introduction
!
! Returns the first half of the array `x` if `section == 1`, the second half
! of the array `x` if `section == 2`, and an empty array otherwise. If `size
! (x) == 1`,  `split(x, 1)`  returns and empty array,  and `split(x, 2)`
! returns `x(1)`.

INTERFACE
  MODULE PURE FUNCTION split_char(x, section) RESULT(Ans)
    CHARACTER(LEN=*), INTENT(IN) :: x !! Input array
    INTEGER(I4B), INTENT(IN) :: section !! Array section to return
    CHARACTER(LEN=:), ALLOCATABLE :: Ans
  END FUNCTION split_char
END INTERFACE

INTERFACE SPLIT
  MODULE PROCEDURE split_int, split_char, split_real
END INTERFACE SPLIT

PUBLIC :: SPLIT
