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

!> Test: DiagIndx compared with a scan of the matrix
! summary: For several shapes (m,n) and every diagNo, DiagIndx(m,n,k) must
! list, in order, all entries (i,j) with j - i = k, and its number of rows
! must equal DiagSize(m,n,k).

PROGRAM main
USE GlobalData, ONLY: I4B
USE DiagUtility, ONLY: DiagIndx
USE DiagUtility, ONLY: DiagSize
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: shapes(2, 5) = RESHAPE( &
                           [1, 1, 3, 3, 4, 7, 7, 4, 2, 9], [2, 5])
INTEGER(I4B), ALLOCATABLE :: ind(:, :)
INTEGER(I4B) :: ishape, m, n, k, ii, jj, cnt
LOGICAL :: ok_size, ok_values

ok_size = .TRUE.
ok_values = .TRUE.

DO ishape = 1, SIZE(shapes, 2)
  m = shapes(1, ishape)
  n = shapes(2, ishape)
  DO k = -m - 2, n + 2
    ind = DiagIndx(m, n, k)

    IF (SIZE(ind, 1) .NE. DiagSize(m, n, k) .OR. SIZE(ind, 2) .NE. 2) THEN
      ok_size = .FALSE.
      CYCLE
    END IF

    ! scan the matrix row by row; entries of a diagonal appear
    ! from the top-left to the bottom-right
    cnt = 0
    DO ii = 1, m
      DO jj = 1, n
        IF (jj - ii .EQ. k) THEN
          cnt = cnt + 1
          IF (cnt .GT. SIZE(ind, 1)) THEN
            ok_values = .FALSE.
          ELSE IF (ind(cnt, 1) .NE. ii .OR. ind(cnt, 2) .NE. jj) THEN
            ok_values = .FALSE.
          END IF
        END IF
      END DO
    END DO
    IF (cnt .NE. SIZE(ind, 1)) ok_values = .FALSE.
  END DO
END DO

CALL OK(ok_size, "DiagIndx shape equals (DiagSize, 2)")
CALL OK(ok_values, "DiagIndx lists all entries with j - i = k")

END PROGRAM main
