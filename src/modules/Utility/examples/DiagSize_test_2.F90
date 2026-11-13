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

!> Test: DiagSize compared with counting the entries of a matrix
! summary: For several shapes (m,n) and every diagNo, DiagSize(m,n,k) must
! equal the number of entries (i,j) of the matrix with j - i = k.

PROGRAM main
USE GlobalData, ONLY: I4B
USE DiagUtility, ONLY: DiagSize
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: shapes(2, 5) = RESHAPE( &
                           [1, 1, 3, 3, 4, 7, 7, 4, 2, 9], [2, 5])
INTEGER(I4B) :: ishape, m, n, k, ii, jj, cnt
LOGICAL :: ok_all

ok_all = .TRUE.
DO ishape = 1, SIZE(shapes, 2)
  m = shapes(1, ishape)
  n = shapes(2, ishape)
  DO k = -m - 2, n + 2
    cnt = 0
    DO jj = 1, n
      DO ii = 1, m
        IF (jj - ii .EQ. k) cnt = cnt + 1
      END DO
    END DO
    IF (DiagSize(m, n, k) .NE. cnt) ok_all = .FALSE.
  END DO
END DO

CALL OK(ok_all, "DiagSize equals number of entries with j - i = k")

END PROGRAM main
