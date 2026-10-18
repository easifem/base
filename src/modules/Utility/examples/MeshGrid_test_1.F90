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

!> Test: MeshGrid (2D)
! summary: grid of 4 x 3 points, x(i,j) = xgv(i) and y(i,j) = ygv(j)

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GridPointUtility, ONLY: MeshGrid
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), ALLOCATABLE :: x(:, :), y(:, :)
REAL(DFP), PARAMETER :: xgv(4) = [0.0_DFP, 1.0_DFP, 2.0_DFP, 3.0_DFP]
REAL(DFP), PARAMETER :: ygv(3) = [5.0_DFP, 6.0_DFP, 7.0_DFP]
INTEGER(I4B) :: ii, jj
LOGICAL :: isOkX, isOkY

CALL MeshGrid(x, y, xgv, ygv)

CALL OK(ALLOCATED(x) .AND. ALLOCATED(y), "x and y are allocated")
CALL OK(ALL(SHAPE(x) .EQ. [4, 3]), "shape of x is (nx, ny)")
CALL OK(ALL(SHAPE(y) .EQ. [4, 3]), "shape of y is (nx, ny)")

isOkX = .TRUE.
isOkY = .TRUE.
DO jj = 1, 3
  DO ii = 1, 4
    isOkX = isOkX .AND. (x(ii, jj) .EQ. xgv(ii))
    isOkY = isOkY .AND. (y(ii, jj) .EQ. ygv(jj))
  END DO
END DO
CALL OK(isOkX, "x(i,j) = xgv(i)")
CALL OK(isOkY, "y(i,j) = ygv(j)")

! each column of x is xgv, each row of y is ygv
CALL OK(ALL(x(:, 2) .EQ. xgv), "column of x")
CALL OK(ALL(y(3, :) .EQ. ygv), "row of y")

END PROGRAM main
