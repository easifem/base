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
! summary: diagonal matrix, no rotation is needed
!
! For a diagonal matrix the eigenvalues are the diagonal entries sorted in
! the decreasing order, and the eigenvectors are the permuted unit vectors.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE EigenUtility, ONLY: GetSymEigenJacobi
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 3
REAL(DFP) :: mat(n, n), w(n), v(n, n), expectedV(n, n)

mat = 0.0_DFP
mat(1, 1) = 1.0_DFP
mat(2, 2) = 3.0_DFP
mat(3, 3) = 2.0_DFP

w = 0.0_DFP
v = 0.0_DFP
CALL GetSymEigenJacobi(mat=mat, eigenValues=w, eigenVectors=v, &
                       maxIter=10_I4B)

! eigenvectors: e2 for 3, e3 for 2, e1 for 1
expectedV = 0.0_DFP
expectedV(2, 1) = 1.0_DFP
expectedV(3, 2) = 1.0_DFP
expectedV(1, 3) = 1.0_DFP

CALL OK(ALL(ABS(w - [3.0_DFP, 2.0_DFP, 1.0_DFP]) .LE. 1.0E-12_DFP), &
        "eigenvalues")
CALL OK(ALL(ABS(v - expectedV) .LE. 1.0E-12_DFP), "eigenvectors")

END PROGRAM main
