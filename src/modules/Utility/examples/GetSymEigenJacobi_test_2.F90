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

!> Test: GetSymEigenJacobi
! summary: eigenvalues of the 4 by 4 second difference matrix
!
! The eigenvalues of the n by n second difference matrix are
! 2 - 2 cos(k pi / (n+1)), k = 1, ..., n. Here mat is not changed.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE EigenUtility, ONLY: GetSymEigenJacobi
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: pi = ACOS(-1.0_DFP)
INTEGER(I4B), PARAMETER :: n = 4
REAL(DFP) :: mat(n, n), matCopy(n, n), w(n), v(n, n), exact(n), h
INTEGER(I4B) :: ii

mat = 0.0_DFP
DO ii = 1, n
  mat(ii, ii) = 2.0_DFP
END DO
DO ii = 1, n - 1
  mat(ii, ii + 1) = -1.0_DFP
  mat(ii + 1, ii) = -1.0_DFP
END DO
matCopy = mat

! decreasing order, k = n, n-1, ..., 1
h = pi / REAL(n + 1, DFP)
DO ii = 1, n
  exact(ii) = 2.0_DFP - 2.0_DFP * COS(REAL(n + 1 - ii, DFP) * h)
END DO

w = 0.0_DFP
v = 0.0_DFP
CALL GetSymEigenJacobi(mat=mat, eigenValues=w, eigenVectors=v, &
                       maxIter=50_I4B)

CALL OK(ALL(ABS(w - exact) .LE. 1.0E-8_DFP), "eigenvalues")
CALL OK(ALL(mat .EQ. matCopy), "mat is not changed")

END PROGRAM main
