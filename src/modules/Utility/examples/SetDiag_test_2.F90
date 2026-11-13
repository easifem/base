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

!> Test: SetDiag with a constant diagonal
! summary: If SIZE(d) == 1, all entries of the diagonal are set to d(1).

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE DiagUtility, ONLY: SetDiag
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 5
INTEGER(I4B), PARAMETER :: kk(5) = [0, 1, -1, 2, -4]
REAL(DFP), PARAMETER :: val = 7.0_DFP
REAL(DFP) :: mat(n, n), expected(n, n)
INTEGER(I4B) :: ik, k, ii, jj
CHARACTER(LEN=32) :: msg

DO ik = 1, SIZE(kk)
  k = kk(ik)
  mat = 0.0_DFP

  CALL SetDiag(mat, [val], k)

  expected = 0.0_DFP
  DO jj = 1, n
    DO ii = 1, n
      IF (jj - ii .EQ. k) expected(ii, jj) = val
    END DO
  END DO

  WRITE (msg, "(A,I0)") "SetDiag constant, diagNo = ", k
  CALL OK(ALL(mat .EQ. expected), TRIM(msg))
END DO

! successive calls accumulate on different diagonals
mat = 0.0_DFP
CALL SetDiag(mat, [1.0_DFP], 0)
CALL SetDiag(mat, [2.0_DFP], 1)
CALL SetDiag(mat, [3.0_DFP], -1)
CALL OK(mat(2, 2) .EQ. 1.0_DFP .AND. mat(2, 3) .EQ. 2.0_DFP &
        .AND. mat(3, 2) .EQ. 3.0_DFP .AND. mat(1, 3) .EQ. 0.0_DFP, &
        "SetDiag successive calls")

END PROGRAM main
