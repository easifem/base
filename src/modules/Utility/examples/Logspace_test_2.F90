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

!> Test: Logspace
! summary: the base option and the endPoint option
!
! If endPoint is false, then the point base**b is not included, and the
! exponents are spaced by (b - a) / N.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GridPointUtility, ONLY: Logspace
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), ALLOCATABLE :: x(:)

! base 2, the end point is included
x = Logspace(a=0.0_DFP, b=3.0_DFP, N=4_I4B, base=2_I4B)
CALL OK(SIZE(x) .EQ. 4, "base 2: size")
CALL OK(ALL(ABS(x - [1.0_DFP, 2.0_DFP, 4.0_DFP, 8.0_DFP]) .LE. tol), &
        "base 2: values")

! base 2, the end point is not included
x = Logspace(a=0.0_DFP, b=4.0_DFP, N=4_I4B, endPoint=.FALSE., &
             base=2_I4B)
CALL OK(SIZE(x) .EQ. 4, "endPoint=.FALSE.: size")
CALL OK(ALL(ABS(x - [1.0_DFP, 2.0_DFP, 4.0_DFP, 8.0_DFP]) .LE. tol), &
        "endPoint=.FALSE.: values, 16 is not included")

! default base is 10
x = Logspace(a=0.0_DFP, b=3.0_DFP, N=3_I4B, endPoint=.FALSE.)
CALL OK(ALL(ABS(x - [1.0_DFP, 10.0_DFP, 100.0_DFP]) .LE. tol), &
        "default base is 10")

! negative exponents
x = Logspace(a=-2.0_DFP, b=0.0_DFP, N=3_I4B)
CALL OK(ALL(ABS(x - [0.01_DFP, 0.1_DFP, 1.0_DFP]) .LE. tol), &
        "negative exponents")

END PROGRAM main
