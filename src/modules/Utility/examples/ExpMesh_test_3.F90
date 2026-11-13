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
! summary: uniform mesh (a = 1) and a mesh with one element (N = 1)

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GridPointUtility, ONLY: ExpMesh
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: x(5), z(2)

! a = 1 gives a uniform mesh
x = ExpMesh(rmin=1.0_DFP, rmax=5.0_DFP, a=1.0_DFP, N=4_I4B)
CALL OK(ALL(ABS(x - [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP]) &
            .LE. tol), "a = 1 gives a uniform mesh")

! uniform mesh with negative end points
x = ExpMesh(rmin=-1.0_DFP, rmax=1.0_DFP, a=1.0_DFP, N=4_I4B)
CALL OK(ALL(ABS(x - [-1.0_DFP, -0.5_DFP, 0.0_DFP, 0.5_DFP, 1.0_DFP]) &
            .LE. tol), "uniform mesh on [-1, 1]")

! a single element, the ratio a has no effect
z = ExpMesh(rmin=2.0_DFP, rmax=6.0_DFP, a=1.0_DFP, N=1_I4B)
CALL OK(ALL(ABS(z - [2.0_DFP, 6.0_DFP]) .LE. tol), "N = 1, a = 1")

z = ExpMesh(rmin=2.0_DFP, rmax=6.0_DFP, a=5.0_DFP, N=1_I4B)
CALL OK(ALL(ABS(z - [2.0_DFP, 6.0_DFP]) .LE. tol), "N = 1, a = 5")

END PROGRAM main
