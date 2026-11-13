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

!> Test: Convert (rank-6 to rank-4)
! summary: For each (a,b), from(:,:,:,:,a,b) is converted to a rank-2
! block-matrix, i.e.
! to((c-1)*n1+i, (d-1)*n2+j, a, b) = from(i,j,c,d,a,b).
! Shape of to is (n1*n3, n2*n4, n5, n6).

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ConvertUtility, ONLY: Convert
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
INTEGER(I4B), PARAMETER :: n1 = 2, n2 = 3, n3 = 2, n4 = 4, n5 = 3, n6 = 2
REAL(DFP) :: from(n1, n2, n3, n4, n5, n6)
REAL(DFP) :: expected(n1 * n3, n2 * n4, n5, n6)
REAL(DFP), ALLOCATABLE :: to(:, :, :, :)
INTEGER(I4B) :: ii, jj, cc, dd, aa, bb

from = RESHAPE([(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, &
                 ii=1, SIZE(from))], SHAPE(from))

DO bb = 1, n6
  DO aa = 1, n5
    DO dd = 1, n4
      DO cc = 1, n3
        DO jj = 1, n2
          DO ii = 1, n1
            expected((cc - 1) * n1 + ii, (dd - 1) * n2 + jj, aa, bb) = &
              from(ii, jj, cc, dd, aa, bb)
          END DO
        END DO
      END DO
    END DO
  END DO
END DO

CALL Convert(from=from, to=to)

CALL OK(ALL(SHAPE(to) .EQ. [n1 * n3, n2 * n4, n5, n6]), &
        "Convert rank6 shape")
CALL OK(MAXVAL(ABS(to - expected)) .LT. tol, "Convert rank6 to rank4")
CALL OK(ABS(to(1, 1, n5, n6) - from(1, 1, 1, 1, n5, n6)) .LT. tol, &
        "Convert rank6 corner entry")
CALL OK(ABS(to(n1 * n3, n2 * n4, 1, 1) &
            - from(n1, n2, n3, n4, 1, 1)) .LT. tol, &
        "Convert rank6 last block entry")

END PROGRAM main
