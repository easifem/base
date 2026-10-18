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

!> Test: DiagSize with hand-computed values
! summary: Sizes of the diagonals of a (4,4) and a (4,7) matrix, including
! diagonals that lie outside the matrix (size 0).

PROGRAM main
USE GlobalData, ONLY: I4B
USE DiagUtility, ONLY: DiagSize
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: k

! square (4,4), DiagSize(n, diagNo)
CALL OK(DiagSize(4, 0) .EQ. 4, "DiagSize(4, 0)")
CALL OK(DiagSize(4, 1) .EQ. 3, "DiagSize(4, 1)")
CALL OK(DiagSize(4, 2) .EQ. 2, "DiagSize(4, 2)")
CALL OK(DiagSize(4, 3) .EQ. 1, "DiagSize(4, 3)")
CALL OK(DiagSize(4, 5) .EQ. 0, "DiagSize(4, 5) out of bound")
CALL OK(DiagSize(4, -1) .EQ. 3, "DiagSize(4, -1)")
CALL OK(DiagSize(4, -2) .EQ. 2, "DiagSize(4, -2)")
CALL OK(DiagSize(4, -3) .EQ. 1, "DiagSize(4, -3)")
CALL OK(DiagSize(4, -5) .EQ. 0, "DiagSize(4, -5) out of bound")

! the (n, diagNo) form agrees with the (m, n, diagNo) form
DO k = -6, 6
  IF (DiagSize(4, k) .NE. DiagSize(4, 4, k)) EXIT
END DO
CALL OK(k .EQ. 7, "DiagSize(n, k) == DiagSize(n, n, k)")

! rectangle (4,7), super-diagonals 0..7
CALL OK(DiagSize(4, 7, 0) .EQ. 4, "DiagSize(4, 7, 0)")
CALL OK(DiagSize(4, 7, 1) .EQ. 4, "DiagSize(4, 7, 1)")
CALL OK(DiagSize(4, 7, 2) .EQ. 4, "DiagSize(4, 7, 2)")
CALL OK(DiagSize(4, 7, 3) .EQ. 4, "DiagSize(4, 7, 3)")
CALL OK(DiagSize(4, 7, 4) .EQ. 3, "DiagSize(4, 7, 4)")
CALL OK(DiagSize(4, 7, 5) .EQ. 2, "DiagSize(4, 7, 5)")
CALL OK(DiagSize(4, 7, 6) .EQ. 1, "DiagSize(4, 7, 6)")
CALL OK(DiagSize(4, 7, 7) .EQ. 0, "DiagSize(4, 7, 7) out of bound")

! rectangle (4,7), sub-diagonals
CALL OK(DiagSize(4, 7, -1) .EQ. 3, "DiagSize(4, 7, -1)")
CALL OK(DiagSize(4, 7, -2) .EQ. 2, "DiagSize(4, 7, -2)")
CALL OK(DiagSize(4, 7, -3) .EQ. 1, "DiagSize(4, 7, -3)")
CALL OK(DiagSize(4, 7, -4) .EQ. 0, "DiagSize(4, 7, -4) out of bound")

! rectangle (7,4)
CALL OK(DiagSize(7, 4, 0) .EQ. 4, "DiagSize(7, 4, 0)")
CALL OK(DiagSize(7, 4, 1) .EQ. 3, "DiagSize(7, 4, 1)")
CALL OK(DiagSize(7, 4, 3) .EQ. 1, "DiagSize(7, 4, 3)")
CALL OK(DiagSize(7, 4, 4) .EQ. 0, "DiagSize(7, 4, 4) out of bound")
CALL OK(DiagSize(7, 4, -1) .EQ. 4, "DiagSize(7, 4, -1)")
CALL OK(DiagSize(7, 4, -3) .EQ. 4, "DiagSize(7, 4, -3)")
CALL OK(DiagSize(7, 4, -4) .EQ. 3, "DiagSize(7, 4, -4)")
CALL OK(DiagSize(7, 4, -6) .EQ. 1, "DiagSize(7, 4, -6)")
CALL OK(DiagSize(7, 4, -7) .EQ. 0, "DiagSize(7, 4, -7) out of bound")

END PROGRAM main
