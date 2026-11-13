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

!> Test: ExpMesh
! summary: exponential mesh with a > 1, the elements grow
!
! For a = 4 and N = 3 on [0, 7], the element sizes are 1, 2, 4.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GridPointUtility, ONLY: ExpMesh
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: x(4), y(11), h(10)

x = ExpMesh(rmin=0.0_DFP, rmax=7.0_DFP, a=4.0_DFP, N=3_I4B)

CALL OK(SIZE(x) .EQ. 4, "number of nodes is N + 1")
CALL OK(ALL(ABS(x - [0.0_DFP, 1.0_DFP, 3.0_DFP, 7.0_DFP]) .LE. tol), &
        "nodes")
CALL OK(ABS(x(1)) .LE. tol, "left end is rmin")
CALL OK(ABS(x(4) - 7.0_DFP) .LE. tol, "right end is rmax")
CALL OK(ABS((x(4) - x(3)) / (x(2) - x(1)) - 4.0_DFP) .LE. tol, &
        "largest to smallest element is a")

! a longer mesh on [1, 100] with a = 10
y = ExpMesh(rmin=1.0_DFP, rmax=100.0_DFP, a=10.0_DFP, N=10_I4B)
h = y(2:11) - y(1:10)

CALL OK(SIZE(y) .EQ. 11, "N = 10: number of nodes")
CALL OK(ABS(y(1) - 1.0_DFP) .LE. tol, "N = 10: left end")
CALL OK(ABS(y(11) - 100.0_DFP) .LE. 1.0E-10_DFP, "N = 10: right end")
CALL OK(ALL(h .GT. 0.0_DFP), "N = 10: increasing nodes")
CALL OK(ALL(h(2:10) .GT. h(1:9)), "N = 10: elements grow")
CALL OK(ABS(h(10) / h(1) - 10.0_DFP) .LE. 1.0E-10_DFP, &
        "N = 10: largest to smallest element is a")

END PROGRAM main
