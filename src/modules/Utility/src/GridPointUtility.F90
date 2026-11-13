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

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-16
! summary: Routines for generating grid points in 1D, 2D and 3D.
!
! Description:
!   This module contains routines for generating a set of points.
!
!   - `ExpMesh` generates the nodes of an exponential mesh in 1D. The
!     size of the elements grows (or shrinks) geometrically.
!   - `Linspace` generates linearly spaced points in 1D.
!   - `Logspace` generates logarithmically spaced points in 1D.
!   - `MeshGrid` generates the coordinates of the points of a 2D or 3D
!     rectangular grid from the grid vectors.
!
!   These routines are available for `REAL64` and `REAL32`.

MODULE GridPointUtility
USE GlobalData, ONLY: I4B, LGT, DFP
USE GlobalData, ONLY: INT8, INT16, INT32, INT64
USE GlobalData, ONLY: REAL32, REAL64
IMPLICIT NONE

PRIVATE
PUBLIC :: ExpMesh
PUBLIC :: Linspace
PUBLIC :: Logspace
PUBLIC :: MeshGrid

!----------------------------------------------------------------------------
!                                                            ExpMesh@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-16
! summary: Nodes of an exponential mesh in 1D, REAL64 and REAL32.
!
! Description:
!   Returns the `N + 1` nodes of a mesh of `N` elements on the interval
!   [`rmin`, `rmax`]. The size of the elements changes geometrically, and
!   `a` is the ratio of the size of the last element to the size of the
!   first element.
!
!   - If `a > 1`, the elements grow from left to right.
!   - If `0 < a < 1`, the elements shrink from left to right.
!   - If `a = 1`, the mesh is uniform.
!   - If `N = 1`, the result is [`rmin`, `rmax`] for any `a`.
!
!   `N` should be at least 1, and `a` should be positive.
!
! Arguments:
!   rmin : REAL(REAL64) - left end of the 1D domain
!   rmax : REAL(REAL64) - right end of the 1D domain
!   a : REAL(REAL64) - ratio of the largest to the smallest element
!   N : INTEGER(I4B) - number of elements in the mesh
!
! Returns:
!   ans : REAL(REAL64), shape (N+1) - nodes of the mesh
!
! Formula:
!   For $a \ne 1$ and $N > 1$, let
!
!   $$\beta = \frac{\ln a}{N - 1}, \qquad
!   \alpha = \frac{r_{\max} - r_{\min}}{e^{\beta N} - 1}$$
!
!   Then the nodes are
!
!   $$x_i = r_{\min} + \alpha\left(e^{\beta (i-1)} - 1\right),
!   \qquad i = 1, \ldots, N + 1$$
!
!   The size of the i-th element is
!   $h_i = x_{i+1} - x_i = \alpha\,(e^{\beta} - 1)\,e^{\beta (i-1)}$,
!   so that $h_N / h_1 = a$. For $a = 1$,
!
!   $$x_i = r_{\min} + (i-1)\,\frac{r_{\max} - r_{\min}}{N}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GridPointUtility, ONLY: ExpMesh
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP) :: x(4), y(11), h(10)
!
! x = ExpMesh(rmin=0.0_DFP, rmax=7.0_DFP, a=4.0_DFP, N=3_I4B)
!
! CALL OK(SIZE(x) .EQ. 4, "number of nodes is N + 1")
! CALL OK(ALL(ABS(x - [0.0_DFP, 1.0_DFP, 3.0_DFP, 7.0_DFP]) .LE. tol), &
!   "nodes")
! CALL OK(ABS(x(1)) .LE. tol, "left end is rmin")
! CALL OK(ABS(x(4) - 7.0_DFP) .LE. tol, "right end is rmax")
! CALL OK(ABS((x(4) - x(3)) / (x(2) - x(1)) - 4.0_DFP) .LE. tol, &
!   "largest to smallest element is a")
!
! ! a longer mesh on [1, 100] with a = 10
! y = ExpMesh(rmin=1.0_DFP, rmax=100.0_DFP, a=10.0_DFP, N=10_I4B)
! h = y(2:11) - y(1:10)
!
! CALL OK(SIZE(y) .EQ. 11, "N = 10: number of nodes")
! CALL OK(ABS(y(1) - 1.0_DFP) .LE. tol, "N = 10: left end")
! CALL OK(ABS(y(11) - 100.0_DFP) .LE. 1.0E-10_DFP, "N = 10: right end")
! CALL OK(ALL(h .GT. 0.0_DFP), "N = 10: increasing nodes")
! CALL OK(ALL(h(2:10) .GT. h(1:9)), "N = 10: elements grow")
! CALL OK(ABS(h(10) / h(1) - 10.0_DFP) .LE. 1.0E-10_DFP, &
!   "N = 10: largest to smallest element is a")
!
! END PROGRAM main
!```

INTERFACE ExpMesh
  MODULE PURE FUNCTION ExpMesh_Real64(rmin, rmax, a, N) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: rmin
    !! left end of 1D domain
    REAL(REAL64), INTENT(IN) :: rmax
    !! right end of 1D domain
    REAL(REAL64), INTENT(IN) :: a
    !! Ratio of largest to smallest element, a should be positive
    !! a = 1, then we get uniform mesh
    INTEGER(I4B), INTENT(IN) :: N
    !! Number of elements present in mesh
    REAL(REAL64) :: ans(N + 1)
    !! Number of nodes in mesh
  END FUNCTION ExpMesh_Real64

  MODULE PURE FUNCTION ExpMesh_Real32(rmin, rmax, a, N) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: rmin
    !! left end of 1D domain
    REAL(REAL32), INTENT(IN) :: rmax
    !! right end of 1D domain
    REAL(REAL32), INTENT(IN) :: a
    !! Ratio of largest to smallest element, a should be positive
    !! a = 1, then we get uniform mesh
    INTEGER(I4B), INTENT(IN) :: N
    !! Number of elements present in mesh
    REAL(REAL32) :: ans(N + 1)
    !! Number of nodes in mesh
  END FUNCTION ExpMesh_Real32
END INTERFACE ExpMesh

!----------------------------------------------------------------------------
!                                                           Linspace@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-16
! summary: Linearly spaced vector, REAL64 and REAL32.
!
! Description:
!   Returns a vector of `N` linearly spaced points from `a` to `b`,
!   including both end points. If `N` is omitted, then 100 points are
!   returned.
!
!   - If `N = 1`, only `a` is returned.
!   - If `a > b`, the points are decreasing.
!   - The result is allocated by the function.
!   - The last point is computed as $a + (N-1)\,\Delta$, so it can differ
!     from `b` by a round-off error.
!
! Arguments:
!   a : REAL(REAL64) - left end of the 1D domain
!   b : REAL(REAL64) - right end of the 1D domain
!   N : INTEGER(I4B), OPTIONAL - number of points, default is 100
!
! Returns:
!   ans : REAL(REAL64), ALLOCATABLE, shape (N) - points
!
! Formula:
!   $$x_i = a + (i - 1)\,\frac{b - a}{N - 1}, \qquad i = 1, \ldots, N$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GridPointUtility, ONLY: Linspace
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), ALLOCATABLE :: x(:)
! REAL(DFP) :: exact(10)
! INTEGER(I4B) :: ii
!
! DO ii = 1, 10
!   exact(ii) = REAL(ii, DFP)
! END DO
!
! x = Linspace(1.0_DFP, 10.0_DFP, 10_I4B)
!
! CALL OK(ALLOCATED(x), "result is allocated")
! CALL OK(SIZE(x) .EQ. 10, "size is N")
! CALL OK(ALL(ABS(x - exact) .LE. tol), "values")
! CALL OK(ABS(x(1) - 1.0_DFP) .LE. tol, "first point is a")
! CALL OK(ABS(x(10) - 10.0_DFP) .LE. tol, "last point is b")
!
! END PROGRAM main
!```

INTERFACE LinSpace
  MODULE PURE FUNCTION Linspace_Real64(a, b, N) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: a
    !! left end of 1D domain
    REAL(REAL64), INTENT(IN) :: b
    !! right end of 1D domain
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: N
    !! Number of points including a and b
    REAL(REAL64), ALLOCATABLE :: ans(:)
    !! Number of nodes in mesh
  END FUNCTION Linspace_Real64

  MODULE PURE FUNCTION Linspace_Real32(a, b, N) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: a
    !! left end of 1D domain
    REAL(REAL32), INTENT(IN) :: b
    !! right end of 1D domain
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: N
    !! Number of points including a and b
    REAL(REAL32), ALLOCATABLE :: ans(:)
    !! Number of nodes in mesh
  END FUNCTION Linspace_Real32
END INTERFACE LinSpace

!----------------------------------------------------------------------------
!                                                           Logspace@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-16
! summary: Logarithmically spaced vector, REAL64.
!
! Description:
!   Returns a vector of `N` points which are evenly spaced on a
!   logarithmic scale. The first point is `base**a`. The exponents are
!   evenly spaced, so the ratio of two consecutive points is constant.
!
!   - If `endPoint` is true (default), then the last point is `base**b`.
!   - If `endPoint` is false, then `base**b` is not included, and
!     the exponents are spaced by `(b - a) / N`.
!   - The default value of `base` is 10 and of `N` is 100.
!   - The result is allocated by the function.
!
! Arguments:
!   a : REAL(REAL64) - the first point is `base**a`
!   b : REAL(REAL64) - the last point is `base**b` if `endPoint` is true
!   N : INTEGER(I4B), OPTIONAL - number of points, default is 100
!   endPoint : LOGICAL(LGT), OPTIONAL - include the end point, default is
!     true
!   base : INTEGER(I4B), OPTIONAL - base of the log scale, default is 10
!
! Returns:
!   ans : REAL(REAL64), ALLOCATABLE, shape (N) - points
!
! Formula:
!   $$x_i = \mathrm{base}^{\,a + (i - 1)\,\Delta}, \qquad
!   i = 1, \ldots, N$$
!
!   where
!
!   $$\Delta = \frac{b - a}{N - 1} \ (\mathrm{endPoint\ true}), \qquad
!   \Delta = \frac{b - a}{N} \ (\mathrm{endPoint\ false})$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GridPointUtility, ONLY: Logspace
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), ALLOCATABLE :: x(:)
! REAL(DFP) :: exact(10)
! INTEGER(I4B) :: ii
!
! DO ii = 1, 10
!   exact(ii) = 10.0_DFP**ii
! END DO
!
! x = Logspace(a=1.0_DFP, b=10.0_DFP, N=10_I4B)
!
! CALL OK(ALLOCATED(x), "result is allocated")
! CALL OK(SIZE(x) .EQ. 10, "size is N")
! CALL OK(ALL(ABS(x - exact) .LE. 1.0E-10_DFP * exact), "values")
!
! ! the same result when endPoint and base are given
! x = Logspace(a=1.0_DFP, b=10.0_DFP, N=10_I4B, endPoint=.TRUE., &
!   base=10_I4B)
! CALL OK(ALL(ABS(x - exact) .LE. 1.0E-10_DFP * exact), &
!   "endPoint=.TRUE., base=10")
!
! END PROGRAM main
!```

INTERFACE Logspace
  MODULE PURE FUNCTION Logspace_Real64(a, b, N, endPoint, base) RESULT(Ans)
    REAL(REAL64), INTENT(IN) :: a
    !! left end of 1D domain
    REAL(REAL64), INTENT(IN) :: b
    !! right end of 1D domain
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: N
    !! Number of points including a and b
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: endPoint
    !! default is true, if true then include endpoint
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: base
    !! default is 10
    REAL(REAL64), ALLOCATABLE :: ans(:)
    !! Number of nodes in mesh
  END FUNCTION Logspace_Real64

  MODULE PURE FUNCTION Logspace_Real32(a, b, N, endPoint, base) RESULT(Ans)
    REAL(REAL32), INTENT(IN) :: a
    !! left end of 1D domain
    REAL(REAL32), INTENT(IN) :: b
    !! right end of 1D domain
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: N
    !! Number of points including a and b, default is 100
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: endPoint
    !! default is true, if true then include endpoint
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: base
    !! default is 10
    REAL(REAL32), ALLOCATABLE :: ans(:)
    !! Number of nodes in mesh
  END FUNCTION Logspace_Real32
END INTERFACE Logspace

!----------------------------------------------------------------------------
!                                                            MeshGrid@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-16
! summary: Coordinates of the points of a 2D rectangular grid, REAL64.
!
! Description:
!   Generates the coordinates of the points of a 2D rectangular grid
!   from the grid vectors `xgv` and `ygv`.
!
!   - `x` and `y` are allocated (or reshaped) by this routine to the
!     shape `(nx, ny)`, where `nx = SIZE(xgv)` and `ny = SIZE(ygv)`.
!   - The coordinates of the point `(i, j)` are `[x(i, j), y(i, j)]`.
!   - Each column of `x` is a copy of `xgv`.
!   - Each row of `y` is a copy of `ygv`.
!
!   Note that the first index of `x` and `y` is the index of the x
!   direction (matrix indexing, not the Cartesian indexing of Matlab).
!
! Arguments:
!   x : REAL(REAL64), ALLOCATABLE, shape (nx, ny), INOUT - x coordinates
!   y : REAL(REAL64), ALLOCATABLE, shape (nx, ny), INOUT - y coordinates
!   xgv : REAL(REAL64), shape (nx) - grid vector in x direction
!   ygv : REAL(REAL64), shape (ny) - grid vector in y direction
!
! Formula:
!   $$x_{ij} = \mathrm{xgv}_i, \qquad y_{ij} = \mathrm{ygv}_j, \qquad
!   1 \le i \le n_x, \quad 1 \le j \le n_y$$
!
!   For example, if `xgv = [0, 1, 2]` and `ygv = [5, 6]`, then
!
!   $$x = \begin{pmatrix} 0 & 0 \\ 1 & 1 \\ 2 & 2 \end{pmatrix}, \qquad
!   y = \begin{pmatrix} 5 & 6 \\ 5 & 6 \\ 5 & 6 \end{pmatrix}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GridPointUtility, ONLY: MeshGrid
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), ALLOCATABLE :: x(:, :), y(:, :)
! REAL(DFP), PARAMETER :: xgv(4) = [0.0_DFP, 1.0_DFP, 2.0_DFP, 3.0_DFP]
! REAL(DFP), PARAMETER :: ygv(3) = [5.0_DFP, 6.0_DFP, 7.0_DFP]
! INTEGER(I4B) :: ii, jj
! LOGICAL :: isOkX, isOkY
!
! CALL MeshGrid(x, y, xgv, ygv)
!
! CALL OK(ALLOCATED(x) .AND. ALLOCATED(y), "x and y are allocated")
! CALL OK(ALL(SHAPE(x) .EQ. [4, 3]), "shape of x is (nx, ny)")
! CALL OK(ALL(SHAPE(y) .EQ. [4, 3]), "shape of y is (nx, ny)")
!
! isOkX = .TRUE.
! isOkY = .TRUE.
! DO jj = 1, 3
!   DO ii = 1, 4
!     isOkX = isOkX .AND. (x(ii, jj) .EQ. xgv(ii))
!     isOkY = isOkY .AND. (y(ii, jj) .EQ. ygv(jj))
!   END DO
! END DO
! CALL OK(isOkX, "x(i,j) = xgv(i)")
! CALL OK(isOkY, "y(i,j) = ygv(j)")
!
! ! each column of x is xgv, each row of y is ygv
! CALL OK(ALL(x(:, 2) .EQ. xgv), "column of x")
! CALL OK(ALL(y(3, :) .EQ. ygv), "row of y")
!
! END PROGRAM main
!```

INTERFACE MeshGrid
  MODULE PURE SUBROUTINE MeshGrid2D_Real64(x, y, xgv, ygv)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: x(:, :)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: y(:, :)
    REAL(REAL64), INTENT(IN) :: xgv(:)
    REAL(REAL64), INTENT(IN) :: ygv(:)
  END SUBROUTINE MeshGrid2D_Real64

  MODULE PURE SUBROUTINE MeshGrid2D_Real32(x, y, xgv, ygv)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: x(:, :)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: y(:, :)
    REAL(REAL32), INTENT(IN) :: xgv(:)
    REAL(REAL32), INTENT(IN) :: ygv(:)
  END SUBROUTINE MeshGrid2D_Real32
END INTERFACE MeshGrid

!----------------------------------------------------------------------------
!                                                           MeshGrid@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-16
! summary: Coordinates of the points of a 3D rectangular grid, REAL64.
!
! Description:
!   Generates the coordinates of the points of a 3D rectangular grid
!   from the grid vectors `xgv`, `ygv` and `zgv`.
!
!   - `x`, `y` and `z` are allocated (or reshaped) by this routine to
!     the shape `(nx, ny, nz)`, where `nx = SIZE(xgv)`, `ny = SIZE(ygv)`
!     and `nz = SIZE(zgv)`.
!   - The coordinates of the point `(i, j, k)` are
!     `[x(i, j, k), y(i, j, k), z(i, j, k)]`.
!
! Arguments:
!   x : REAL(REAL64), ALLOCATABLE, shape (nx, ny, nz), INOUT
!   y : REAL(REAL64), ALLOCATABLE, shape (nx, ny, nz), INOUT
!   z : REAL(REAL64), ALLOCATABLE, shape (nx, ny, nz), INOUT
!   xgv : REAL(REAL64), shape (nx) - grid vector in x direction
!   ygv : REAL(REAL64), shape (ny) - grid vector in y direction
!   zgv : REAL(REAL64), shape (nz) - grid vector in z direction
!
! Formula:
!   $$x_{ijk} = \mathrm{xgv}_i, \qquad y_{ijk} = \mathrm{ygv}_j, \qquad
!   z_{ijk} = \mathrm{zgv}_k$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GridPointUtility, ONLY: MeshGrid
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), ALLOCATABLE :: x(:, :, :), y(:, :, :), z(:, :, :)
! REAL(DFP), PARAMETER :: xgv(3) = [1.0_DFP, 2.0_DFP, 3.0_DFP]
! REAL(DFP), PARAMETER :: ygv(2) = [4.0_DFP, 5.0_DFP]
! REAL(DFP), PARAMETER :: zgv(4) = [6.0_DFP, 7.0_DFP, 8.0_DFP, 9.0_DFP]
! INTEGER(I4B) :: ii, jj, kk
! LOGICAL :: isOkX, isOkY, isOkZ
!
! CALL MeshGrid(x, y, z, xgv, ygv, zgv)
!
! CALL OK(ALLOCATED(x) .AND. ALLOCATED(y) .AND. ALLOCATED(z), &
!   "x, y and z are allocated")
! CALL OK(ALL(SHAPE(x) .EQ. [3, 2, 4]), "shape of x is (nx, ny, nz)")
! CALL OK(ALL(SHAPE(y) .EQ. [3, 2, 4]), "shape of y is (nx, ny, nz)")
! CALL OK(ALL(SHAPE(z) .EQ. [3, 2, 4]), "shape of z is (nx, ny, nz)")
!
! isOkX = .TRUE.
! isOkY = .TRUE.
! isOkZ = .TRUE.
! DO kk = 1, 4
!   DO jj = 1, 2
!     DO ii = 1, 3
!       isOkX = isOkX .AND. (x(ii, jj, kk) .EQ. xgv(ii))
!       isOkY = isOkY .AND. (y(ii, jj, kk) .EQ. ygv(jj))
!       isOkZ = isOkZ .AND. (z(ii, jj, kk) .EQ. zgv(kk))
!     END DO
!   END DO
! END DO
! CALL OK(isOkX, "x(i,j,k) = xgv(i)")
! CALL OK(isOkY, "y(i,j,k) = ygv(j)")
! CALL OK(isOkZ, "z(i,j,k) = zgv(k)")
!
! END PROGRAM main
!```

INTERFACE MeshGrid
  MODULE PURE SUBROUTINE MeshGrid3D_Real64(x, y, z, xgv, ygv, zgv)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: x(:, :, :)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: y(:, :, :)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: z(:, :, :)
    REAL(REAL64), INTENT(IN) :: xgv(:)
    REAL(REAL64), INTENT(IN) :: ygv(:)
    REAL(REAL64), INTENT(IN) :: zgv(:)
  END SUBROUTINE MeshGrid3D_Real64

  MODULE PURE SUBROUTINE MeshGrid3D_Real32(x, y, z, xgv, ygv, zgv)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: x(:, :, :)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: y(:, :, :)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: z(:, :, :)
    REAL(REAL32), INTENT(IN) :: xgv(:)
    REAL(REAL32), INTENT(IN) :: ygv(:)
    REAL(REAL32), INTENT(IN) :: zgv(:)
  END SUBROUTINE MeshGrid3D_Real32
END INTERFACE MeshGrid

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GridPointUtility
