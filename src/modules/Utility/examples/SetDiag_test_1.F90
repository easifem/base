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

!> Test: SetDiag with a vector of REAL(DFP)
! summary: SetDiag sets the diagonal k to d and leaves every other entry
! unchanged. The k-th entry of d goes to position min(i,j) along the
! diagonal j - i = k.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE DiagUtility, ONLY: SetDiag
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 5
REAL(DFP), PARAMETER :: base = 1.0_DFP
REAL(DFP) :: mat(n, n), expected(n, n)
REAL(DFP), ALLOCATABLE :: d(:)
INTEGER(I4B) :: k, ii, jj
CHARACTER(LEN=32) :: msg

DO k = -3, 3
  mat = base
  d = REAL([(10 + ii, ii=1, n - ABS(k))], DFP)

  CALL SetDiag(mat, d, k)

  expected = base
  DO jj = 1, n
    DO ii = 1, n
      IF (jj - ii .EQ. k) expected(ii, jj) = REAL(10 + MIN(ii, jj), DFP)
    END DO
  END DO

  WRITE (msg, "(A,I0)") "SetDiag diagNo = ", k
  CALL OK(ALL(mat .EQ. expected), TRIM(msg))
END DO

! hand-checked: main diagonal of a 5x5 matrix of ones
mat = base
CALL SetDiag(mat, [2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP], 0)
CALL OK(mat(1, 1) .EQ. 2.0_DFP .AND. mat(5, 5) .EQ. 6.0_DFP &
        .AND. mat(1, 2) .EQ. base .AND. mat(2, 1) .EQ. base, &
        "SetDiag main diagonal, hand check")

END PROGRAM main
