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

!> Test: SymEigenValues2by2
! summary: eigenvalues of 2 by 2 symmetric matrices

PROGRAM main
USE GlobalData, ONLY: DFP
USE EigenUtility, ONLY: SymEigenValues2by2
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: mat(2, 2), w(2), exact(2)

! general matrix, the eigenvalues are 0.5 and 3.0
mat = RESHAPE([2.5_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP], [2, 2])
exact = [0.5_DFP, 3.0_DFP]
w = SymEigenValues2by2(mat)
CALL OK(ALL(ABS(w - exact) .LE. tol), "general matrix")
CALL OK(w(1) .LE. w(2), "ascending order")
CALL OK(ABS(SUM(w) - 3.5_DFP) .LE. tol, "sum is the trace")
CALL OK(ABS(w(1) * w(2) - 1.5_DFP) .LE. tol, "product is the determinant")

! diagonal matrix
mat = RESHAPE([3.0_DFP, 0.0_DFP, 0.0_DFP, -1.0_DFP], [2, 2])
w = SymEigenValues2by2(mat)
CALL OK(ALL(ABS(w - [-1.0_DFP, 3.0_DFP]) .LE. tol), "diagonal matrix")

! repeated eigenvalue
mat = RESHAPE([2.0_DFP, 0.0_DFP, 0.0_DFP, 2.0_DFP], [2, 2])
w = SymEigenValues2by2(mat)
CALL OK(ALL(ABS(w - 2.0_DFP) .LE. tol), "repeated eigenvalue")

END PROGRAM main
