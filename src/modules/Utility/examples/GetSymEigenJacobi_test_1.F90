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
! summary: eigenvalues and eigenvectors of a 5 by 5 symmetric matrix
!
! The eigenvalues are returned in the decreasing order. Each column of
! eigenVectors satisfies mat v = lambda v, and the columns are orthonormal.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE EigenUtility, ONLY: GetSymEigenJacobi
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 5
REAL(DFP) :: mat(n, n), w(n), v(n, n), exact(n), res(n)
REAL(DFP) :: eye(n, n)
INTEGER(I4B) :: jj
LOGICAL :: isOk

mat = RESHAPE([ &
              1.96_DFP, -6.49_DFP, -0.47_DFP, -7.20_DFP, -0.65_DFP, &
              -6.49_DFP, 3.80_DFP, -6.39_DFP, 1.50_DFP, -6.34_DFP, &
              -0.47_DFP, -6.39_DFP, 4.17_DFP, -1.51_DFP, 2.67_DFP, &
              -7.20_DFP, 1.50_DFP, -1.51_DFP, 5.70_DFP, 1.80_DFP, &
              -0.65_DFP, -6.34_DFP, 2.67_DFP, 1.80_DFP, -7.10_DFP], [n, n])
exact = [16.09_DFP, 8.87_DFP, 0.86_DFP, -6.23_DFP, -11.07_DFP]

w = 0.0_DFP
v = 0.0_DFP
CALL GetSymEigenJacobi(mat=mat, eigenValues=w, eigenVectors=v, &
                       maxIter=50_I4B)

CALL OK(ALL(ABS(w - exact) .LE. 1.0E-2_DFP), "eigenvalues")
CALL OK(ALL(w(1:n - 1) .GE. w(2:n)), "decreasing order")

isOk = .TRUE.
DO jj = 1, n
  res = MATMUL(mat, v(:, jj)) - w(jj) * v(:, jj)
  isOk = isOk .AND. (MAXVAL(ABS(res)) .LE. 1.0E-8_DFP)
END DO
CALL OK(isOk, "mat v = lambda v")

eye = MATMUL(TRANSPOSE(v), v)
DO jj = 1, n
  eye(jj, jj) = eye(jj, jj) - 1.0_DFP
END DO
CALL OK(MAXVAL(ABS(eye)) .LE. 1.0E-8_DFP, "orthonormal eigenvectors")

END PROGRAM main
