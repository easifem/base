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
! summary: exponential mesh with 0 < a < 1, the elements shrink
!
! For a = 1/4 and N = 3 on [0, 7], the element sizes are 4, 2, 1.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GridPointUtility, ONLY: ExpMesh
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: x(4)

x = ExpMesh(rmin=0.0_DFP, rmax=7.0_DFP, a=0.25_DFP, N=3_I4B)

CALL OK(ALL(ABS(x - [0.0_DFP, 4.0_DFP, 6.0_DFP, 7.0_DFP]) .LE. tol), &
        "nodes")
CALL OK(ABS((x(2) - x(1)) / (x(4) - x(3)) - 4.0_DFP) .LE. tol, &
        "largest to smallest element is 1/a")
CALL OK(ALL(x(2:4) .GT. x(1:3)), "increasing nodes")

END PROGRAM main
