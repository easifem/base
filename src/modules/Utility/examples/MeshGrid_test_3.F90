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
! summary: arrays which are already allocated, and grid from Linspace
!
! The arrays are reshaped if they have a different shape.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GridPointUtility, ONLY: MeshGrid
USE GridPointUtility, ONLY: Linspace
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), ALLOCATABLE :: x(:, :), y(:, :)
REAL(DFP), ALLOCATABLE :: xgv(:), ygv(:)

! arrays allocated with a wrong shape
ALLOCATE (x(2, 2), y(5, 1))
x = -1.0_DFP
y = -1.0_DFP

xgv = Linspace(0.0_DFP, 1.0_DFP, 3_I4B)
ygv = Linspace(-1.0_DFP, 1.0_DFP, 5_I4B)

CALL MeshGrid(x, y, xgv, ygv)

CALL OK(ALL(SHAPE(x) .EQ. [3, 5]), "x is reshaped")
CALL OK(ALL(SHAPE(y) .EQ. [3, 5]), "y is reshaped")

! the corners of the rectangle [0, 1] x [-1, 1]
CALL OK(ABS(x(1, 1)) .LE. 1.0E-12_DFP .AND. &
        ABS(y(1, 1) + 1.0_DFP) .LE. 1.0E-12_DFP, "corner (1, 1)")
CALL OK(ABS(x(3, 1) - 1.0_DFP) .LE. 1.0E-12_DFP .AND. &
        ABS(y(3, 1) + 1.0_DFP) .LE. 1.0E-12_DFP, "corner (nx, 1)")
CALL OK(ABS(x(1, 5)) .LE. 1.0E-12_DFP .AND. &
        ABS(y(1, 5) - 1.0_DFP) .LE. 1.0E-12_DFP, "corner (1, ny)")
CALL OK(ABS(x(3, 5) - 1.0_DFP) .LE. 1.0E-12_DFP .AND. &
        ABS(y(3, 5) - 1.0_DFP) .LE. 1.0E-12_DFP, "corner (nx, ny)")

END PROGRAM main
