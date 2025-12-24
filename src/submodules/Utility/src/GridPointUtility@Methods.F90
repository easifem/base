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

SUBMODULE(GridPointUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 ExpMesh
!----------------------------------------------------------------------------

MODULE PROCEDURE ExpMesh_Real64
INTEGER(I4B) :: i
REAL(DFP) :: alpha, beta
  !!
IF (ABS(a - 1) .LT. TINY(1.0_DFP)) THEN
  alpha = (rmax - rmin) / N
  DO i = 1, N + 1
    ans(i) = alpha * (i - 1.0_DFP) + rmin
  END DO
ELSE
  IF (N .GT. 1) THEN
    beta = LOG(a) / (N - 1)
    alpha = (rmax - rmin) / (EXP(beta * N) - 1)
    DO i = 1, N + 1
      ans(i) = alpha * (EXP(beta * (i - 1)) - 1) + rmin
    END DO
  ELSE IF (N .EQ. 1) THEN
    ans(1) = rmin
    ans(2) = rmax
  END IF
END IF
  !!
END PROCEDURE ExpMesh_Real64

!----------------------------------------------------------------------------
!                                                                 ExpMesh
!----------------------------------------------------------------------------

MODULE PROCEDURE ExpMesh_Real32
INTEGER(I4B) :: i
REAL(REAL32) :: alpha, beta
  !!
IF (ABS(a - 1) .LT. TINY(1.0_DFP)) THEN
  alpha = (rmax - rmin) / N
  DO i = 1, N + 1
    ans(i) = alpha * (i - 1.0_REAL32) + rmin
  END DO
ELSE
  IF (N .GT. 1) THEN
    beta = LOG(a) / (N - 1)
    alpha = (rmax - rmin) / (EXP(beta * N) - 1)
    DO i = 1, N + 1
      ans(i) = alpha * (EXP(beta * (i - 1)) - 1) + rmin
    END DO
  ELSE IF (N .EQ. 1) THEN
    ans(1) = rmin
    ans(2) = rmax
  END IF
END IF
  !!
END PROCEDURE ExpMesh_Real32

!----------------------------------------------------------------------------
!                                                               Linspace
!----------------------------------------------------------------------------

MODULE PROCEDURE LinSpace_Real32
!   Local vars
REAL(REAL32) :: dx
INTEGER(I4B) :: i
INTEGER(I4B) :: nn
  !! main
nn = INPUT(option=n, default=100)
IF (nn .EQ. 1) THEN
  ans = [a]
ELSE
  ALLOCATE (ans(nn))
  dx = (b - a) / REAL((nn - 1), REAL32)
  ans = [(i * dx + a, i=0, nn - 1)]
END IF
END PROCEDURE LinSpace_Real32

!----------------------------------------------------------------------------
!                                                                 LinSpace
!----------------------------------------------------------------------------

MODULE PROCEDURE LinSpace_Real64
!   Local vars
REAL(REAL64) :: dx
INTEGER(I4B) :: i
INTEGER(I4B) :: nn
!> main
nn = INPUT(option=n, default=100)
IF (nn .EQ. 1) THEN
  ans = [a]
ELSE
  ALLOCATE (ans(nn))
  dx = (b - a) / REAL((nn - 1), REAL64)
  ans = [(i * dx + a, i=0, nn - 1)]
END IF
END PROCEDURE LinSpace_Real64

!----------------------------------------------------------------------------
!                                                               Linspace
!----------------------------------------------------------------------------

MODULE PROCEDURE LogSpace_Real32
INTEGER(I4B) :: base0, n0
LOGICAL(LGT) :: endpoint0
REAL(REAL32), ALLOCATABLE :: ans0(:)
  !!
endpoint0 = INPUT(option=endPoint, default=.TRUE.)
base0 = INPUT(option=base, default=10)
n0 = INPUT(option=n, default=100_I4B)
  !!
IF (endpoint0) THEN
  ans0 = Linspace(a=a, b=b, n=n0)
  ans = base0**(ans0)
ELSE
  ans0 = Linspace(a=a, b=b, n=n0 + 1)
  ans = base0**(ans0(1:n0))
END IF
  !!
IF (ALLOCATED(ans0)) DEALLOCATE (ans0)
END PROCEDURE LogSpace_Real32

!----------------------------------------------------------------------------
!                                                               Linspace
!----------------------------------------------------------------------------

MODULE PROCEDURE LogSpace_Real64
INTEGER(I4B) :: base0, n0
LOGICAL(LGT) :: endpoint0
REAL(REAL64), ALLOCATABLE :: ans0(:)
  !!
endpoint0 = INPUT(option=endPoint, default=.TRUE.)
base0 = INPUT(option=base, default=10)
n0 = INPUT(option=n, default=100_I4B)
  !!
IF (endpoint0) THEN
  ans0 = Linspace(a=a, b=b, n=n0)
  ans = base0**(ans0)
ELSE
  ans0 = Linspace(a=a, b=b, n=n0 + 1)
  ans = base0**(ans0(1:n0))
END IF
  !!
IF (ALLOCATED(ans0)) DEALLOCATE (ans0)
END PROCEDURE LogSpace_Real64

!----------------------------------------------------------------------------
!                                                                 MeshGrid
!----------------------------------------------------------------------------

MODULE PROCEDURE MeshGrid2D_Real64
! Local variables
INTEGER(I4B) :: nx
INTEGER(I4B) :: ny
! Initial setting
nx = SIZE(xgv, dim=1)
ny = SIZE(ygv, dim=1)
CALL Reallocate(x, nx, ny)
CALL Reallocate(y, nx, ny)
x(:, :) = SPREAD(xgv, dim=2, ncopies=ny)
y(:, :) = SPREAD(ygv, dim=1, ncopies=nx)
END PROCEDURE MeshGrid2D_Real64

!----------------------------------------------------------------------------
!                                                                 MeshGrid
!----------------------------------------------------------------------------

MODULE PROCEDURE MeshGrid2D_Real32
! Local variables
INTEGER(I4B) :: nx
INTEGER(I4B) :: ny
! Initial setting
nx = SIZE(xgv, dim=1)
ny = SIZE(ygv, dim=1)
CALL Reallocate(x, nx, ny)
CALL Reallocate(y, nx, ny)
x(:, :) = SPREAD(xgv, dim=2, ncopies=ny)
y(:, :) = SPREAD(ygv, dim=1, ncopies=nx)
END PROCEDURE MeshGrid2D_Real32

!----------------------------------------------------------------------------
!                                                              MeshGrid
!----------------------------------------------------------------------------

MODULE PROCEDURE MeshGrid3D_Real64
INTEGER :: nx, ny, nz, i
nx = SIZE(xgv); ny = SIZE(ygv); nz = SIZE(zgv)
CALL Reallocate(x, nx, ny, nz)
CALL Reallocate(y, nx, ny, nz)
CALL Reallocate(z, nx, ny, nz)
DO i = 1, nz
  x(:, :, i) = SPREAD(xgv, dim=2, ncopies=ny)
  y(:, :, i) = SPREAD(ygv, dim=1, ncopies=nx)
END DO
DO i = 1, nx
  z(i, :, :) = SPREAD(zgv, dim=1, ncopies=ny)
END DO
END PROCEDURE MeshGrid3D_Real64

!----------------------------------------------------------------------------
!                                                              MeshGrid
!----------------------------------------------------------------------------

MODULE PROCEDURE MeshGrid3D_Real32
INTEGER :: nx, ny, nz, i
nx = SIZE(xgv); ny = SIZE(ygv); nz = SIZE(zgv)
CALL Reallocate(x, ny, nx, nz)
CALL Reallocate(y, ny, nx, nz)
CALL Reallocate(z, ny, nx, nz)
DO i = 1, nz
  x(:, :, i) = SPREAD(xgv, dim=2, ncopies=ny)
  y(:, :, i) = SPREAD(ygv, dim=1, ncopies=nx)
END DO
DO i = 1, nx
  z(i, :, :) = SPREAD(zgv, dim=1, ncopies=ny)
END DO
END PROCEDURE MeshGrid3D_Real32

END SUBMODULE Methods
