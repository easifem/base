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

!> Test: Diag (extract a diagonal from a square matrix)
! summary: Diag(mat, k) returns mat(i, i+k) for k >= 0 and mat(i-k, i)
! for k < 0. The matrix is mat(i,j) = 10*i + j, so the entries are easy
! to verify by hand.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE DiagUtility, ONLY: Diag
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
INTEGER(I4B), PARAMETER :: n = 5
REAL(DFP) :: mat(n, n)
INTEGER(I4B) :: ii, jj

DO jj = 1, n
  DO ii = 1, n
    mat(ii, jj) = REAL(10 * ii + jj, DFP)
  END DO
END DO

CALL OK(isEqual(Diag(mat, 0), REAL([11, 22, 33, 44, 55], DFP)), &
        "Diag main diagonal")
CALL OK(isEqual(Diag(mat, 1), REAL([12, 23, 34, 45], DFP)), &
        "Diag first super-diagonal")
CALL OK(isEqual(Diag(mat, 2), REAL([13, 24, 35], DFP)), &
        "Diag second super-diagonal")
CALL OK(isEqual(Diag(mat, 4), REAL([15], DFP)), &
        "Diag last super-diagonal")
CALL OK(isEqual(Diag(mat, -1), REAL([21, 32, 43, 54], DFP)), &
        "Diag first sub-diagonal")
CALL OK(isEqual(Diag(mat, -2), REAL([31, 42, 53], DFP)), &
        "Diag second sub-diagonal")
CALL OK(isEqual(Diag(mat, -4), REAL([51], DFP)), &
        "Diag last sub-diagonal")

CONTAINS

PURE FUNCTION isEqual(a, b) RESULT(ans)
  REAL(DFP), INTENT(IN) :: a(:), b(:)
  LOGICAL :: ans
  ans = SIZE(a) .EQ. SIZE(b)
  IF (ans) ans = ALL(ABS(a - b) .LT. tol)
END FUNCTION isEqual

END PROGRAM main
