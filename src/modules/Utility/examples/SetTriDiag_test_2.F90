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

!> Test: SetTriDiag with constant diagonals
! summary: If the size of d, da or db is 1, the respective diagonal is
! filled with that single value.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE DiagUtility, ONLY: SetTriDiag
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 4
REAL(DFP) :: mat(n, n), expected(n, n)
INTEGER(I4B) :: ii

! the classical [-1, 2, -1] matrix
mat = 0.0_DFP
CALL SetTriDiag(mat=mat, d=[2.0_DFP], da=[-1.0_DFP], db=[-1.0_DFP])

! rows of the hand-written matrix, stored column by column
expected = RESHAPE([2, -1, 0, 0, &
                    -1, 2, -1, 0, &
                    0, -1, 2, -1, &
                    0, 0, -1, 2], [n, n])

CALL OK(ALL(mat .EQ. expected), "SetTriDiag constant [-1, 2, -1]")

! mixed: vector main diagonal, constant off-diagonals
mat = 0.0_DFP
CALL SetTriDiag(mat=mat, d=[1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], &
                da=[5.0_DFP], db=[-5.0_DFP])

expected = 0.0_DFP
DO ii = 1, n
  expected(ii, ii) = REAL(ii, DFP)
END DO
DO ii = 1, n - 1
  expected(ii, ii + 1) = 5.0_DFP
  expected(ii + 1, ii) = -5.0_DFP
END DO

CALL OK(ALL(mat .EQ. expected), "SetTriDiag mixed vector and constant")

END PROGRAM main
