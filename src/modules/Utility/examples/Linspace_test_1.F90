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
! summary: ten equally spaced points on [1, 10]

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GridPointUtility, ONLY: Linspace
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), ALLOCATABLE :: x(:)
REAL(DFP) :: exact(10)
INTEGER(I4B) :: ii

DO ii = 1, 10
  exact(ii) = REAL(ii, DFP)
END DO

x = Linspace(1.0_DFP, 10.0_DFP, 10_I4B)

CALL OK(ALLOCATED(x), "result is allocated")
CALL OK(SIZE(x) .EQ. 10, "size is N")
CALL OK(ALL(ABS(x - exact) .LE. tol), "values")
CALL OK(ABS(x(1) - 1.0_DFP) .LE. tol, "first point is a")
CALL OK(ABS(x(10) - 10.0_DFP) .LE. tol, "last point is b")

END PROGRAM main
