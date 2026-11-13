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

!> Test: Convert (rank-4 to rank-2)
! summary: to((a-1)*n1+i, (b-1)*n2+j) = from(i,j,a,b), so that
! from(:,:,a,b) is the (a,b) block of to. Shape of to is (n1*n3, n2*n4).

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ConvertUtility, ONLY: Convert
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
INTEGER(I4B), PARAMETER :: n1 = 2, n2 = 3, n3 = 4, n4 = 5
REAL(DFP) :: from(n1, n2, n3, n4)
REAL(DFP) :: expected(n1 * n3, n2 * n4)
REAL(DFP), ALLOCATABLE :: to(:, :)
INTEGER(I4B) :: ii, jj, aa, bb

from = RESHAPE([(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, &
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

CALL Convert(from=from, to=to)

CALL OK(ALL(SHAPE(to) .EQ. [n1 * n3, n2 * n4]), "Convert rank4 shape")
CALL OK(MAXVAL(ABS(to - expected)) .LT. tol, "Convert rank4 to rank2")

! block checks
CALL OK(ABS(to(1, 1) - from(1, 1, 1, 1)) .LT. tol, "Convert rank4 block(1,1)")
CALL OK(ABS(to(n1 + 1, 1) - from(1, 1, 2, 1)) .LT. tol, &
        "Convert rank4 block(2,1)")
CALL OK(ABS(to(1, n2 + 1) - from(1, 1, 1, 2)) .LT. tol, &
        "Convert rank4 block(1,2)")
CALL OK(ABS(to(n1 * n3, n2 * n4) - from(n1, n2, n3, n4)) .LT. tol, &
        "Convert rank4 last entry")

END PROGRAM main
