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

!> Test: Diag (make a diagonal matrix from an INTEGER(I4B) vector)
! summary: Diag([1,2,3,4,5]) is the matrix with 1,2,3,4,5 on the diagonal

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE DiagUtility, ONLY: Diag
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 5
INTEGER(I4B) :: d(n)
REAL(DFP) :: mat(n, n), expected(n, n)
INTEGER(I4B) :: ii

d = [1, 2, 3, 4, 5]

! hand-written matrix, stored column by column (symmetric)
expected = RESHAPE([1, 0, 0, 0, 0, &
                    0, 2, 0, 0, 0, &
                    0, 0, 3, 0, 0, &
                    0, 0, 0, 4, 0, &
                    0, 0, 0, 0, 5], [n, n])

mat = Diag(d)

CALL OK(ALL(SHAPE(mat) .EQ. [n, n]), "Diag(I4B) shape")
CALL OK(ALL(mat .EQ. expected), "Diag(I4B) values")

! the diagonal is recovered by Diag(mat, 0)
CALL OK(ALL(Diag(mat, 0) .EQ. REAL(d, DFP)), "Diag(I4B) round trip")

! a vector of size 1 gives a 1x1 matrix
ii = 7
mat(1:1, 1:1) = Diag([ii])
CALL OK(mat(1, 1) .EQ. 7.0_DFP, "Diag(I4B) size one")

END PROGRAM main
