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

!> Test: DiagIndx with hand-computed values
! summary: Indices (row, column) of diagonals of a (4,4) and a (4,7) matrix.

PROGRAM main
USE GlobalData, ONLY: I4B
USE DiagUtility, ONLY: DiagIndx
USE Test_Method, ONLY: OK
IMPLICIT NONE

! (4,4)
CALL OK(isEqual(DiagIndx(4, 4, 0), &
                RESHAPE([1, 2, 3, 4, 1, 2, 3, 4], [4, 2])), &
        "DiagIndx(4,4, 0)")
CALL OK(isEqual(DiagIndx(4, 4, 1), &
                RESHAPE([1, 2, 3, 2, 3, 4], [3, 2])), &
        "DiagIndx(4,4, 1)")
CALL OK(isEqual(DiagIndx(4, 4, 2), &
                RESHAPE([1, 2, 3, 4], [2, 2])), &
        "DiagIndx(4,4, 2)")
CALL OK(isEqual(DiagIndx(4, 4, -1), &
                RESHAPE([2, 3, 4, 1, 2, 3], [3, 2])), &
        "DiagIndx(4,4, -1)")
CALL OK(isEqual(DiagIndx(4, 4, -2), &
                RESHAPE([3, 4, 1, 2], [2, 2])), &
        "DiagIndx(4,4, -2)")

! (4,7)
CALL OK(isEqual(DiagIndx(4, 7, 3), &
                RESHAPE([1, 2, 3, 4, 4, 5, 6, 7], [4, 2])), &
        "DiagIndx(4,7, 3)")
CALL OK(isEqual(DiagIndx(4, 7, 4), &
                RESHAPE([1, 2, 3, 5, 6, 7], [3, 2])), &
        "DiagIndx(4,7, 4)")
CALL OK(isEqual(DiagIndx(4, 7, 6), &
                RESHAPE([1, 7], [1, 2])), &
        "DiagIndx(4,7, 6)")
CALL OK(isEqual(DiagIndx(4, 7, -1), &
                RESHAPE([2, 3, 4, 1, 2, 3], [3, 2])), &
        "DiagIndx(4,7, -1)")
CALL OK(isEqual(DiagIndx(4, 7, -3), &
                RESHAPE([4, 1], [1, 2])), &
        "DiagIndx(4,7, -3)")

! out of bound diagonals have no entries
CALL OK(SIZE(DiagIndx(4, 7, 7), 1) .EQ. 0, "DiagIndx(4,7, 7) is empty")
CALL OK(SIZE(DiagIndx(4, 7, -4), 1) .EQ. 0, "DiagIndx(4,7, -4) is empty")

CONTAINS

PURE FUNCTION isEqual(a, b) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: a(:, :), b(:, :)
  LOGICAL :: ans
  ans = ALL(SHAPE(a) .EQ. SHAPE(b))
  IF (ans) ans = ALL(a .EQ. b)
END FUNCTION isEqual

END PROGRAM main
