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
! summary: default N (100 points) and the constant ratio of the points

PROGRAM main
USE GlobalData, ONLY: DFP
USE GridPointUtility, ONLY: Logspace
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), ALLOCATABLE :: x(:)
REAL(DFP) :: ratio

x = Logspace(a=0.0_DFP, b=2.0_DFP)

CALL OK(SIZE(x) .EQ. 100, "default N is 100")
CALL OK(ABS(x(1) - 1.0_DFP) .LE. 1.0E-10_DFP, "first point is 10**a")
CALL OK(ABS(x(100) - 100.0_DFP) .LE. 1.0E-8_DFP, "last point is 10**b")

! consecutive points have a constant ratio
ratio = x(2) / x(1)
CALL OK(ALL(ABS(x(2:100) / x(1:99) - ratio) .LE. 1.0E-10_DFP), &
        "constant ratio")
CALL OK(ALL(x(2:100) .GT. x(1:99)), "increasing points")

END PROGRAM main
