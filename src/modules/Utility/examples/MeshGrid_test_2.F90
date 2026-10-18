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
! along with this program.  If not, see <https://www.gnu.org/licenses/>

!> Test: MeshGrid (3D)
! summary: grid of 3 x 2 x 4 points, x(i,j,k) = xgv(i), y = ygv(j), z = zgv(k)

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GridPointUtility, ONLY: MeshGrid
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), ALLOCATABLE :: x(:, :, :), y(:, :, :), z(:, :, :)
REAL(DFP), PARAMETER :: xgv(3) = [1.0_DFP, 2.0_DFP, 3.0_DFP]
REAL(DFP), PARAMETER :: ygv(2) = [4.0_DFP, 5.0_DFP]
REAL(DFP), PARAMETER :: zgv(4) = [6.0_DFP, 7.0_DFP, 8.0_DFP, 9.0_DFP]
INTEGER(I4B) :: ii, jj, kk
LOGICAL :: isOkX, isOkY, isOkZ

CALL MeshGrid(x, y, z, xgv, ygv, zgv)

CALL OK(ALLOCATED(x) .AND. ALLOCATED(y) .AND. ALLOCATED(z), &
        "x, y and z are allocated")
CALL OK(ALL(SHAPE(x) .EQ. [3, 2, 4]), "shape of x is (nx, ny, nz)")
CALL OK(ALL(SHAPE(y) .EQ. [3, 2, 4]), "shape of y is (nx, ny, nz)")
CALL OK(ALL(SHAPE(z) .EQ. [3, 2, 4]), "shape of z is (nx, ny, nz)")

isOkX = .TRUE.
isOkY = .TRUE.
isOkZ = .TRUE.
DO kk = 1, 4
  DO jj = 1, 2
    DO ii = 1, 3
      isOkX = isOkX .AND. (x(ii, jj, kk) .EQ. xgv(ii))
      isOkY = isOkY .AND. (y(ii, jj, kk) .EQ. ygv(jj))
      isOkZ = isOkZ .AND. (z(ii, jj, kk) .EQ. zgv(kk))
    END DO
  END DO
END DO
CALL OK(isOkX, "x(i,j,k) = xgv(i)")
CALL OK(isOkY, "y(i,j,k) = ygv(j)")
CALL OK(isOkZ, "z(i,j,k) = zgv(k)")

END PROGRAM main
