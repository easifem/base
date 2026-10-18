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

!> Test: SymEigenValues3by3
! summary: a general symmetric matrix with all different eigenvalues
!
! The characteristic polynomial of the matrix is
! lambda^3 - 4 lambda^2 - 31 lambda + 118.

PROGRAM main
USE GlobalData, ONLY: DFP
USE EigenUtility, ONLY: SymEigenValues3by3
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-4_DFP
REAL(DFP) :: mat(3, 3), w(3), exact(3)

mat = RESHAPE([ &
              -2.0_DFP, -4.0_DFP, 2.0_DFP, &
              -4.0_DFP, 1.0_DFP, 2.0_DFP, &
              2.0_DFP, 2.0_DFP, 5.0_DFP], [3, 3])
exact = [-5.51082_DFP, 3.65928_DFP, 5.85154_DFP]

w = SymEigenValues3by3(mat)

CALL OK(ALL(ABS(w - exact) .LE. tol), "eigenvalues")
CALL OK(ABS(SUM(w) - 4.0_DFP) .LE. tol, "sum is the trace")
CALL OK(ABS(w(1) * w(2) * w(3) + 118.0_DFP) .LE. 1.0E-2_DFP, &
        "product is the determinant")

END PROGRAM main
