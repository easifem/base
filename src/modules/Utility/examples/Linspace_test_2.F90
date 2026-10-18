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

!> Test: Linspace
! summary: default N, N = 1, N = 2, decreasing and negative ranges

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GridPointUtility, ONLY: Linspace
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), ALLOCATABLE :: x(:)
REAL(DFP) :: h

! N is omitted, 100 points are returned
x = Linspace(0.0_DFP, 99.0_DFP)
CALL OK(SIZE(x) .EQ. 100, "default N is 100")
CALL OK(ABS(x(1)) .LE. tol, "default N: first point")
CALL OK(ABS(x(100) - 99.0_DFP) .LE. tol, "default N: last point")
CALL OK(ABS(x(50) - 49.0_DFP) .LE. tol, "default N: middle point")

! N = 1, only the left end is returned
x = Linspace(3.0_DFP, 8.0_DFP, 1_I4B)
CALL OK(SIZE(x) .EQ. 1, "N = 1: size")
CALL OK(ABS(x(1) - 3.0_DFP) .LE. tol, "N = 1: value is a")

! N = 2, both end points
x = Linspace(3.0_DFP, 8.0_DFP, 2_I4B)
CALL OK(SIZE(x) .EQ. 2, "N = 2: size")
CALL OK(ALL(ABS(x - [3.0_DFP, 8.0_DFP]) .LE. tol), "N = 2: values")

! a > b, decreasing points
x = Linspace(5.0_DFP, 1.0_DFP, 5_I4B)
CALL OK(ALL(ABS(x - [5.0_DFP, 4.0_DFP, 3.0_DFP, 2.0_DFP, 1.0_DFP]) &
            .LE. tol), "decreasing range")

! negative range, spacing is constant
x = Linspace(-1.0_DFP, 1.0_DFP, 9_I4B)
h = 0.25_DFP
CALL OK(ALL(ABS(x(2:9) - x(1:8) - h) .LE. tol), "constant spacing")
CALL OK(ABS(x(5)) .LE. tol, "middle point is zero")

END PROGRAM main
