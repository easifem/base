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
! summary: ten points from 10^1 to 10^10, endPoint is true by default

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GridPointUtility, ONLY: Logspace
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), ALLOCATABLE :: x(:)
REAL(DFP) :: exact(10)
INTEGER(I4B) :: ii

DO ii = 1, 10
  exact(ii) = 10.0_DFP**ii
END DO

x = Logspace(a=1.0_DFP, b=10.0_DFP, N=10_I4B)

CALL OK(ALLOCATED(x), "result is allocated")
CALL OK(SIZE(x) .EQ. 10, "size is N")
CALL OK(ALL(ABS(x - exact) .LE. 1.0E-10_DFP * exact), "values")

! the same result when endPoint and base are given
x = Logspace(a=1.0_DFP, b=10.0_DFP, N=10_I4B, endPoint=.TRUE., &
             base=10_I4B)
CALL OK(ALL(ABS(x - exact) .LE. 1.0E-10_DFP * exact), &
        "endPoint=.TRUE., base=10")

END PROGRAM main
