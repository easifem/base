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

!> Test: TriDiag, standard tridiagonal matrix
! summary: d = 1, da = 2 (above), db = -2 (below), a 5x5 matrix.
! The expected matrix is written by hand.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE DiagUtility, ONLY: TriDiag
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 5
REAL(DFP) :: d(n), da(n - 1), db(n - 1)
REAL(DFP) :: amat(n, n), expected(n, n)

d = 1.0_DFP
da = 2.0_DFP
db = -2.0_DFP

! RESHAPE fills column by column, so the rows written below are
! transposed to get the matrix in the natural (row-wise) reading order
expected = TRANSPOSE(RESHAPE([ &
                             1, 2, 0, 0, 0, &
                             -2, 1, 2, 0, 0, &
                             0, -2, 1, 2, 0, &
                             0, 0, -2, 1, 2, &
                             0, 0, 0, -2, 1], [n, n]))

amat = TriDiag(d=d, da=da, db=db, diagNo=1_I4B)
CALL OK(ALL(SHAPE(amat) .EQ. [n, n]), "TriDiag shape")
CALL OK(ALL(amat .EQ. expected), "TriDiag diagNo = 1")

amat = TriDiag(d=d, da=da, db=db)
CALL OK(ALL(amat .EQ. expected), "TriDiag default diagNo")

END PROGRAM main
