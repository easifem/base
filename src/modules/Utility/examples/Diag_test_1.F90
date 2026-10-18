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

!> Test: Diag (make a diagonal matrix from a REAL64 vector)
! summary: ans(i,j) = a(i) if i == j, 0 otherwise

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: REAL64
USE DiagUtility, ONLY: Diag
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 5
REAL(REAL64) :: d(n)
REAL(DFP) :: mat(n, n), expected(n, n)
INTEGER(I4B) :: ii

d = [8.25_REAL64, 3.5_REAL64, 0.5_REAL64, 0.25_REAL64, 2.75_REAL64]

expected = 0.0_DFP
DO ii = 1, n
  expected(ii, ii) = REAL(d(ii), DFP)
END DO

mat = Diag(d)

CALL OK(ALL(SHAPE(mat) .EQ. [n, n]), "Diag(REAL64) shape")
CALL OK(ALL(mat .EQ. expected), "Diag(REAL64) values")

! off-diagonal entries are exactly zero: clear the diagonal, rest is zero
DO ii = 1, n
  mat(ii, ii) = 0.0_DFP
END DO
CALL OK(ALL(mat .EQ. 0.0_DFP), "Diag(REAL64) off-diagonal is zero")

END PROGRAM main
