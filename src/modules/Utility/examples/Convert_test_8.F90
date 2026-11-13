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

!> Test: Convert_ (rank-4 to rank-2), no allocation
! summary: Same mapping as Convert, but written into a pre-allocated,
! larger array. nrow = n1*n3, ncol = n2*n4, and entries outside
! to(1:nrow, 1:ncol) must stay untouched.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ConvertUtility, ONLY: Convert_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: sentinel = 99.0_DFP
INTEGER(I4B), PARAMETER :: n1 = 2, n2 = 3, n3 = 4, n4 = 5
REAL(DFP) :: from(n1, n2, n3, n4)
REAL(DFP) :: expected(n1 * n3, n2 * n4)
REAL(DFP) :: to(n1 * n3 + 3, n2 * n4 + 2)
INTEGER(I4B) :: ii, jj, aa, bb, nrow, ncol

from = RESHAPE([(REAL(MOD(5 * ii + 2, 17) - 8, DFP) / 3.0_DFP, &
                 ii=1, SIZE(from))], SHAPE(from))

DO bb = 1, n4
  DO aa = 1, n3
    DO jj = 1, n2
      DO ii = 1, n1
        expected((aa - 1) * n1 + ii, (bb - 1) * n2 + jj) = &
          from(ii, jj, aa, bb)
      END DO
    END DO
  END DO
END DO

to = sentinel
nrow = -1
ncol = -1
CALL Convert_(from=from, to=to, nrow=nrow, ncol=ncol)

CALL OK(nrow .EQ. n1 * n3, "Convert_ rank4 nrow")
CALL OK(ncol .EQ. n2 * n4, "Convert_ rank4 ncol")
CALL OK(MAXVAL(ABS(to(1:nrow, 1:ncol) - expected)) .LT. tol, &
        "Convert_ rank4 values")
CALL OK(ALL(to(nrow + 1:, :) .EQ. sentinel), &
        "Convert_ rank4 rows beyond nrow untouched")
CALL OK(ALL(to(:, ncol + 1:) .EQ. sentinel), &
        "Convert_ rank4 cols beyond ncol untouched")

END PROGRAM main
